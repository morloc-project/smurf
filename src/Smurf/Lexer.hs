module Smurf.Lexer (
    typename
  , space'
  , line
  , keyword
  , nonSpace
  , chop
  , name
  , path
  , comma
  , pathElement
) where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Control.Monad.State
import qualified Data.Char as DC

-- | a legal non-generic type name
typename :: Parser String
typename = do
  s <- satisfy DC.isUpper
  ss <- many alphaNum
  spaces
  return (s : ss)

-- | match any non-space character
nonSpace :: Parser Char
nonSpace = noneOf " \n\t\r\v"

name :: Parser String
name = do
  s <- satisfy DC.isAlpha
  ss <- many (alphaNum <|> oneOf "._'")
  spaces
  return (s : ss)

pathElement :: Parser String
pathElement = do
  s <- satisfy DC.isAlpha
  ss <- many (noneOf ". \n\t\r\v")
  spaces
  return (s : ss)

comma :: Parser String
comma = do
  c <- char ','
  ss <- many (satisfy DC.isSpace)
  return (c : ss)

path :: Parser [String]
path = do
  ps <- sepBy pathElement (char '.')
  spaces
  return ps

-- | matches all trailing space
chop :: Parser String
chop = do
  ss <- many space'
  optional newline
  return ss

-- | matches for keyword
keyword :: String -> Parser String
keyword w = do
  s <- string w
  spaces
  return s
  
-- | non-newline space
space' :: Parser Char
space' = char ' ' <|> char '\t'

-- | a raw line with spaces
line :: Parser String
line = do
  s <- many1 space'
  l <- many (noneOf "\n")
  newline
  return $ (s ++ l)
