module Smurf.Lexer (
    typename
  , space'
  , line
  , keyword
  , nonSpace
  , chop
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
