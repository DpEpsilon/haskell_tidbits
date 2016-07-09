import Data.Char
data Token = VariableToken [Char]
           | AbstractionToken [Char] Token
           | ApplicationToken Token Token
            deriving (Show)

data Term = Variable Int
          | Abstraction Term
          | Application Term Term
            deriving (Show, Eq)

parseRestOfVariable []   = ([], [])
parseRestOfVariable (x:xs) = if (isAlphaNum x || x == '_')
                             then (x:(fst (parseRestOfVariable xs)), snd (parseRestOfVariable xs))
                             else ([], x:xs)

parseVariable :: [Char] -> Maybe (Token, [Char])

parseVariable [] = Nothing
parseVariable ('\n':xs) = parseVariable xs
parseVariable (' ':xs) = parseVariable xs
parseVariable (x:xs) = if (isAlpha x || x == '_')
                       then Just (VariableToken (x:fst (parseRestOfVariable xs)), snd (parseRestOfVariable xs))
                       else Nothing

parseAbstractionWithVar :: ([Char], [Char]) -> Maybe (Token, [Char])

parseAbstractionWithVar (var, '.':xs) =
    case parseTerm xs of
      Nothing           -> Nothing
      Just (term, rest) -> Just (AbstractionToken var term, rest)

parseAbstractionWithVar ([], _) = Nothing
parseAbstractionWithVar (_, []) = Nothing
parseAbstractionWithVar (_, _)  = Nothing

skipWhitespace (' ':xs)  = skipWhitespace xs
skipWhitespace ('\n':xs) = skipWhitespace xs
skipWhitespace xs        = xs

parseAbstraction :: [Char] -> Maybe (Token, [Char])

parseAbstraction [] = Nothing
parseAbstraction (' ':xs) = parseAbstraction xs
parseAbstraction ('\n':xs) = parseAbstraction xs
parseAbstraction (x:xs) = if x == '\\'
                          then parseAbstractionWithVar (parseRestOfVariable (skipWhitespace xs))
                          else Nothing

parseApplication :: [Char] -> Maybe (Token, [Char])

parseApplication ('(':xs) = do
  (t1, r1) <- parseTerm xs
  (t2, r2) <- parseTerm r1
  if head (skipWhitespace r2) == ')'
  then Just (ApplicationToken t1 t2, r2)
  else Nothing

parseApplication xs = do
  (t1, r1) <- parseVariable xs
  (t2, r2) <- parseTerm r1
  Just (ApplicationToken t1 t2, r2)

parseTerm :: [Char] -> Maybe (Token, [Char])

parseTerm []        = Nothing
parseTerm (' ':xs)  = parseTerm xs
parseTerm ('\n':xs) = parseTerm xs
parseTerm ('(':xs)  =
    case parseTerm xs of
      Just (t, r) -> if head (skipWhitespace r) == ')'
                     then Just (t, r) else Nothing
      Nothing -> Nothing

parseTerm xs =
    case parseAbstraction xs of
      Just (t, r) -> Just (t, r)
      Nothing -> case parseApplication xs of
                   Just (t, r) -> Just (t, r)
                   Nothing -> case parseVariable xs of
                                Just (t, r) -> Just (t, r)
                                Nothing -> Nothing

parseProgram xs =
    case parseTerm xs of
      Just (t, r) -> if skipWhitespace r == ""
                     then Just t
                     else Nothing
      Nothing -> Nothing