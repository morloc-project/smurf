module Smurf.Parser (smurf) where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Control.Monad.State

import Smurf.Data
import qualified Smurf.Lexer as Tok

smurf :: Parser [Top]
smurf = do
  Tok.whiteSpace
  result <- many topPiece 
  eof
  return result

topPiece :: Parser Top
topPiece =
      try aTopSource 
  <|> try aTopStatement
  <|> try aTopImport
  <?> "Top"

aTopSource :: Parser Top
aTopSource = do
  s <- aSource
  return $ TopSource s

aTopStatement :: Parser Top
aTopStatement = do
  s <- aStatement
  return $ TopStatement s

aTopImport :: Parser Top
aTopImport = do
  i <-  try aRestrictedImport
    <|> try aSimpleImport
  return $ TopImport i

aStatement :: Parser Statement
aStatement = do
  s <-  try aSignature
    <|> try aDeclaration
  Tok.op ";" -- for now, every statement ends in a semicolon
  return $ s

aSimpleImport :: Parser Import
aSimpleImport = do
  Tok.reserved "import" 
  path <- Tok.path
  qual <- optionMaybe (Tok.op "as" >> Tok.name)
  return $ Import path qual Nothing

aRestrictedImport :: Parser Import
aRestrictedImport = do
  Tok.reserved "from"
  path <- Tok.path
  Tok.reserved "import"
  -- TODO: I am also importing ontologies, how should that be handled?
  -- TODO: at very least, I am also importing types
  functions <- Tok.parens (sepBy1 Tok.name Tok.comma)
  return $ Import path Nothing (Just functions)

-- | parses a 'source' header, returning the language
aSource :: Parser Source
aSource = do
  Tok.reserved "source"
  lang <- many1 Tok.nonSpace
  Tok.chop
  source <- many Tok.line
  Tok.whiteSpace
  return $ Source lang source

aDeclaration :: Parser Statement
aDeclaration = do
  varname <- Tok.name
  Tok.op "="
  value <- anExpression
  return $ Declaration varname value 

anExpression :: Parser Expression
anExpression =
      try (Tok.parens anExpression)
  <|> try anApplication
  <|> try aPrimitiveExpr
  <?> "an expression"

aPrimitiveExpr :: Parser Expression
aPrimitiveExpr = do
  x <- aPrimitive
  return $ ExprPrimitive x

aPrimitive :: Parser Primitive
aPrimitive =
      try Tok.float -- this must go before integer
  <|> try Tok.integer
  <|> try Tok.boolean
  <|> try Tok.stringLiteral
  <?> "a primitive"

anApplication :: Parser Expression
anApplication = do
  function <- Tok.name
  arguments <- sepBy anExpression Tok.whiteSpace
  return $ ExprApplication function arguments


-- | typename :: [input] -> output constraints 
-- TODO: make the output optional
aSignature :: Parser Statement
aSignature = do
  function <- Tok.name
  Tok.op "::"
  inputs <- sepBy1 Tok.typename Tok.comma
  Tok.op "->"
  output <- Tok.typename
  constraints <- option [] (
      Tok.reserved "where" >>
      Tok.braces (sepBy1 aBooleanExpr (Tok.op ";"))
    )
  return $ Signature function inputs output constraints

aBooleanExpr :: Parser BExpr
aBooleanExpr =
      try (Tok.parens aBooleanExpr)
  <|> try aRelativeExpr
  <|> try aBooleanBinOp
  -- <|> try Tok.name (sepBy Tok.name whitespace)
  -- <|> try Tok.boolean
  <?> "an expression that reduces to True/False"

aBooleanBinOp :: Parser BExpr
aBooleanBinOp = do
  a <- aBooleanExpr
  op <- Tok.relativeBinOp
  b <- aBooleanExpr
  return $ BExprBBinOp op a b

aRelativeExpr :: Parser BExpr
aRelativeExpr = do
  a <- anArithmeticExpr
  op <- Tok.arithmeticBinOp
  b <- anArithmeticExpr
  return $ BExprRBinOp op a b

anArithmeticExpr :: Parser AExpr
anArithmeticExpr = do
  -- TODO fill in other possibilities
  x <- Tok.integer
  return $ toExpr x
  where
    toExpr :: Primitive -> AExpr
    toExpr (PrimitiveInt x) = AExprInt x
    toExpr (PrimitiveReal x) = AExprReal x
    toExpr _ = undefined
