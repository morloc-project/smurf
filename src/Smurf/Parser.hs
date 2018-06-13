{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

parens :: _ -> Parser a
parens p = do
  x <- between (char '(') (char ')') p
  Tok.whiteSpace
  return x

braces :: _ -> Parser a
braces = between (char '{') (char '}')

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
  functions <- parens (sepBy1 Tok.name Tok.comma)
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
anExpression = undefined

-- | typename :: [input] -> output constraints 
aSignature :: Parser Statement
aSignature = do
  typename <- Tok.typename
  Tok.op "::"
  inputs <- sepBy1 constraint Tok.comma
  Tok.op "->"
  output <- Tok.typename
  constraints <- option [] (
      Tok.reserved "where" >>
      braces (sepBy1 constraint (Tok.op ";"))
    )
  return $ Signature typename inputs output constraints

constraint :: Parser String
constraint = do
  -- TODO - replace with real constraint parser
  s <- many1 alphaNum
  Tok.whiteSpace 
  return s
