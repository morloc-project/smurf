module Smurf.Lexer
    ( Parser
    , integer
    , float
    , stringLiteral
    , boolean

    , integerP
    , floatP
    , stringLiteralP
    , booleanP

    , op
    , reserved
    , name
    , tag
    , specificType
    , genericType
    , nonSpace
    , lexeme
    , whiteSpace
    , path
    , comma
    , parens
    , braces
    , brackets
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char as DC
import Data.Void

import qualified Smurf.Data as D
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

whiteSpace :: Parser ()
whiteSpace =
    do
        skipSome (oneOf " \n\t\r\v")
            -- <|> L.skipLineComment "--"
            -- <|> L.skipBlockCommentNested "{-" "-}"
        return ()

surround :: Parser a -> Parser a -> Parser b -> Parser b
surround a b p =
    do
        lexeme a
        v <- lexeme p
        lexeme b
        return v

brackets :: Parser a -> Parser a
brackets = surround (char '[') (char ']')

parens :: Parser a -> Parser a
parens = surround (char '(') (char ')')

braces :: Parser a -> Parser a
braces = surround (char '{') (char '}')

op :: String -> Parser ()
op s =
    do
        string s
        return ()

reserved :: String -> Parser ()
reserved s =
    do
        string s
        return ()

reservedNames :: [String]
reservedNames = [
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

name :: Parser String
name = lexeme $
    do
        head <- letterChar <|> char '_'
        tail <- many $ alphaNumChar <|> oneOf "_.'"
        let v = head : tail
        if v `elem` reservedNames then
            fail "used reserved word as an identifier"
        else
            return v

comma :: Parser ()
comma = lexeme $ do
    char ','
    return ()

integer :: Parser Integer
integer = lexeme $
    do
        num <- many digitChar
        return $ read num

float :: Parser Double
float = lexeme L.float

tag p =
  option "" (try tag')
  where
    tag' = do
        l <- lexeme $ some alphaNumChar
        lexeme $ op ":"
        lookAhead $ lexeme p
        return l

stringLiteral :: Parser String
stringLiteral = lexeme $ do
  _ <- char '"'
  s <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  _ <- char '"'
  return s

boolean :: Parser Bool 
boolean = lexeme $ do
  s <- string "True" <|> string "False"
  return (read s :: Bool)

integerP :: Parser D.Primitive
integerP = lexeme $ do
  D.PrimitiveInt <$> integer

floatP :: Parser D.Primitive
floatP = lexeme $ do
  D.PrimitiveReal <$> float

stringLiteralP :: Parser D.Primitive 
stringLiteralP = lexeme $ do
  D.PrimitiveString <$> stringLiteral

booleanP :: Parser D.Primitive
booleanP = lexeme $ do
  D.PrimitiveBool <$> boolean

-- | a legal non-generic type name
specificType :: Parser String
specificType = lexeme $ do
  s <- satisfy DC.isUpper
  ss <- many alphaNumChar
  return $ s : ss

genericType :: Parser String
genericType = lexeme $
    do
        head <- satisfy (\c -> ('a' <= c && c <= 'z') || c == '_')
        tail <- many $ alphaNumChar <|> oneOf "_.'"
        return $ head : tail

-- | match any non-space character
nonSpace :: Parser Char
nonSpace = noneOf " \n\t\r\v"

path :: Parser [String]
path = lexeme $ sepBy name (char '/')

