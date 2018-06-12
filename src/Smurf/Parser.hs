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
parens = between (char '(') (char ')')

aStatement :: Parser Statement
aStatement = do
  s <- aSignature
  return $ s

aSimpleImport :: Parser Import
aSimpleImport = do
  Tok.keyword "import" 
  path <- Tok.path
  spaces
  qual <- optionMaybe (Tok.keyword "as" >> Tok.pathElement)
  return $ Import path qual Nothing

aRestrictedImport :: Parser Import
aRestrictedImport = do
  Tok.keyword "from"
  path <- Tok.path
  Tok.keyword "import"
  functions <- parens (sepBy1 Tok.name Tok.comma)
  spaces
  return $ Import path Nothing (Just functions)

-- | parses a 'source' header, returning the language
aSource :: Parser Source
aSource = do
  Tok.keyword "source"
  lang <- many1 Tok.nonSpace
  Tok.chop
  source <- many Tok.line
  spaces
  return $ Source lang source

-- | typename :: [input] -> output constraints 
aSignature :: Parser Statement
aSignature = do
  typename <- Tok.typename
  Tok.keyword "::"
  inputs <- sepBy1 constraint Tok.comma
  Tok.keyword "->"
  output <- Tok.typename
  constraints <- option [] (
      Tok.keyword "where" >>
      sepBy1 constraint Tok.comma
    )
  return $ Signature typename inputs output constraints

constraint :: Parser String
constraint = do
  -- TODO - replace with real constraint parser
  s <- many1 alphaNum
  spaces
  return s
