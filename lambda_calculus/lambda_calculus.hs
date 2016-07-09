import Data.Char
import Data.List
data Token = VariableToken [Char]
           | AbstractionToken [Char] Token
           | ApplicationToken Token Token
            deriving (Show)

data Term = Variable Int
          | Abstraction Term
          | Application Term Term
            deriving (Show, Eq)

parseVariable :: [Char] -> Maybe (Token, [Char])
parseAbstraction :: [Char] -> Maybe (Token, [Char])
parseApplication :: [Char] -> Maybe (Token, [Char])
parseTerm :: [Char] -> Maybe (Token, [Char])

parseAbstractionWithVar :: ([Char], [Char]) -> Maybe (Token, [Char])

parseRestOfVariable []   = ([], [])
parseRestOfVariable (x:xs) = if (isAlphaNum x || x == '_')
                             then (x:(fst (parseRestOfVariable xs)), snd (parseRestOfVariable xs))
                             else ([], x:xs)

parseVariable [] = Nothing
parseVariable ('\n':xs) = parseVariable xs
parseVariable (' ':xs) = parseVariable xs
parseVariable (x:xs) = if (isAlpha x || x == '_')
                       then Just (VariableToken (x:fst (parseRestOfVariable xs)), snd (parseRestOfVariable xs))
                       else Nothing

parseAbstractionWithVar (var, '.':xs) =
    case parseTerm xs of
      Nothing           -> Nothing
      Just (term, rest) -> Just (AbstractionToken var term, rest)

parseAbstractionWithVar ([], _) = Nothing
parseAbstractionWithVar (_, []) = Nothing
parseAbstractionWithVar (_, _)  = Nothing

parseAbstraction [] = Nothing
parseAbstraction (' ':xs) = parseAbstraction xs
parseAbstraction ('\n':xs) = parseAbstraction xs
parseAbstraction (x:xs) = if x == '\\'
                          then parseAbstractionWithVar (parseRestOfVariable (skipWhitespace xs))
                          else Nothing

parseApplication ('(':xs) = do
  (t1, r1) <- parseTerm xs
  (t2, r2) <- parseTerm r1
  if head (skipWhitespace r2) == ')'
  then Just (ApplicationToken t1 t2, tail (skipWhitespace r2))
  else Nothing

parseApplication xs = do
  (t1, r1) <- parseVariable xs
  (t2, r2) <- parseTerm r1
  Just (ApplicationToken t1 t2, r2)

parseTerm []        = Nothing
parseTerm (' ':xs)  = parseTerm xs
parseTerm ('\n':xs) = parseTerm xs
parseTerm ('(':xs)  =
    case parseTerm xs of
      Just (t, r) -> if head (skipWhitespace r) == ')'
                     then Just (t, tail (skipWhitespace r)) else Nothing
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

convertProgram :: Token -> [[Char]] -> Maybe Term

convertProgram (VariableToken v) context =
    case elemIndex v context of
      Just i -> Just (Variable i)
      Nothing -> Nothing
convertProgram (AbstractionToken v token) context =
    case convertProgram token (v:context) of
      Just t -> Just (Abstraction t)
      Nothing -> Nothing
convertProgram (ApplicationToken t1 t2) context =
    case (convertProgram t1 context, convertProgram t2 context) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just n1, Just n2) -> Just (Application n1 n2)

convertProgramMaybe :: Maybe Token -> Maybe Term
convertProgramMaybe Nothing = Nothing
convertProgramMaybe (Just t) = (convertProgram t [])

skipWhitespace (' ':xs)  = skipWhitespace xs
skipWhitespace ('\n':xs) = skipWhitespace xs
skipWhitespace xs        = xs
