module Xml where

import Parser

data Attribute = Attribute { key :: String
                           , value :: String
                           } deriving (Eq, Show)


data Node = TagNode { name :: String
                    , attributes :: [Attribute]
                    , children :: [Node]
                    }
          | TextNode { content :: String }
            deriving (Eq, Show)

node :: Parser Node
node = do
  (tagname, attributes) <- openingtag
  children <- many (option node text)
  _ <- closingtag tagname
  return TagNode { name = tagname
                 , attributes = attributes
                 , children = children
                 }

openingtag :: Parser (String, [Attribute])
openingtag = do
  _ <- many (option whitespace eol)
  _ <- char '<'
  tagname <- many1 (nope (sat (`elem` "> /<\n")))
  attributes <- many attribute
  _ <- char '>'
  _ <- many (option whitespace eol)
  return (tagname, attributes)

closingtag :: String -> Parser String
closingtag tagname = do
  _ <- many (option whitespace eol)
  _ <- char '<'
  _ <- char '/'
  name <- string tagname
  _ <- char '>'
  return name

attribute :: Parser Attribute
attribute = do
  _ <- many1 (option whitespace eol)
  key <- many1 (nope (char '='))
  char '='
  value <- surroundedBy (char '"')
  return Attribute { key = key
                   , value = value
                   }

text :: Parser Node
text = do
  _ <- many (option whitespace eol)
  content <- many1 (nope (do
    _ <- many (option whitespace eol)
    char '<'))
  return TextNode { content = content }
