module Parser where

newtype Parser a = Parser { parse :: String -> [(a, String)] }

result :: a -> Parser a
result value = Parser $ \input -> [(value, input)]

zero :: Parser a
zero = Parser $ const []

item :: Parser Char
item = Parser $ \input ->
  case input of
    [] -> []
    (x:xs) -> [(x, xs)]

seq :: Parser a -> Parser b -> Parser (a, b)
parser1 `seq` parser2 = Parser $ \input -> [ ((v, w), input'')
                                           | (v, input') <- parse parser1 input
                                           , (w, input'') <- parse parser2 input' ]

bind :: Parser a -> (a -> Parser b) -> Parser b
parser `bind` function = Parser
                         $ \input ->
                           concatMap (\(parsed, rest) -> parse (function parsed) rest)
                           $ parse parser input

sat :: (Char -> Bool) -> Parser Char
sat predicate = item `bind` \input -> if predicate input
                                      then result input
                                      else zero

char :: Char -> Parser Char
char x = sat (== x)

instance Functor Parser where
  fmap f (Parser results) = Parser (\string -> [ (f result, rest)
                                               | (result, rest) <- results string ])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\string -> [ (f a, rest2)
                                                     | (f, rest1) <- cs1 string
                                                     , (a, rest2) <- cs2 rest1 ])

instance Monad Parser where
  return = result
  (>>=) = bind

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  first <- char c
  rest <- string cs
  return (first:rest)

option :: Parser a -> Parser a -> Parser a
option parser1 parser2 = Parser $ \string ->
  case parse parser1 string of
    [] -> parse parser2 string
    result -> result

many :: Parser a -> Parser [a]
many parser = option (many1 parser) (return [])

many1 :: Parser a -> Parser [a]
many1 parser = do
  result <- parser
  results <- many parser
  return (result:results)

nope :: (Eq a) => Parser a -> Parser Char
nope parser = Parser $ \input ->
  if null (parse parser input)
  then parse (sat (const True)) input
  else []

surroundedBy :: (Eq a) => Parser a -> Parser String
surroundedBy parser = do
  _ <- parser
  result <- many (nope parser)
  _ <- parser
  return result

surroundedBy1 :: (Eq a) => Parser a -> Parser String
surroundedBy1 parser = do
  _ <- parser
  result <- many1 (nope parser)
  _ <- parser
  return result

whitespace :: Parser Char
whitespace = option (char ' ') (char '\t')

eol :: Parser Char
eol = char '\n'
