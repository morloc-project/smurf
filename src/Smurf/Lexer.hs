module Smurf.Lexer (
    integer
  , float
  , stringLiteral
  , boolean
  , whiteSpace
  , op
  , reserved
  , name
  , typename
  , nonSpace
  , path
  , comma
  , chop
  , space'
  , line
) where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Control.Monad.State
import qualified Data.Char as DC
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
  style = Lang.emptyDef {
            Token.commentLine     = "#"
          , Token.commentStart    = ""
          , Token.commentEnd      = ""
          , Token.nestedComments  = False
          , Token.caseSensitive   = True
          , Token.identStart      = letter <|> char '_'
          , Token.identLetter     = alphaNum <|> oneOf "_.'"
          , Token.opStart         = Token.opLetter Lang.emptyDef
          , Token.opLetter        = oneOf ":!$%&*+./<=>?@\\^|-~"
          , Token.reservedOpNames = ["=", "::", "+", "-", "^", "/", "//", "%", "->", ";"]
          , Token.reservedNames = [
                "where"
              , "import"
              , "from"
              , "as"
              , "source"
              , "True"
              , "False"
              , "and"
              , "or"
              , "xor"
              , "nand"
              , "not"
            ]
          }

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

stringLiteral :: Parser String
stringLiteral = do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  return s

boolean :: Parser Bool
boolean = do
  s <- string "True" <|> string "False"
  return (read s)

op :: String -> Parser ()
op = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

name :: Parser String
name = Token.identifier lexer

-- | a legal non-generic type name
typename :: Parser String
typename = do
  s <- satisfy DC.isUpper
  ss <- many alphaNum
  whiteSpace
  return (s : ss)

-- | match any non-space character
nonSpace :: Parser Char
nonSpace = noneOf " \n\t\r\v"

path :: Parser [String]
path = do
  path <- sepBy name (char '/')
  whiteSpace
  return path

comma :: Parser String
comma = Token.comma lexer

-- | matches all trailing space
chop :: Parser String
chop = do
  ss <- many space'
  optional newline
  return ss

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
