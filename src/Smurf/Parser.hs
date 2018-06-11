{-# LANGUAGE FlexibleContexts #-}

module Smurf.Parser (smurf) where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Control.Monad.State

import Smurf.Data
import qualified Smurf.Lexer as Tok

smurf :: Parser Smurf
smurf = do
  result <- many1 aStatement 
  eof
  return $ Smurf result

aStatement :: Parser Statement
aStatement = do
  s <-  try aSignature
    <|> try aSource
    <?> "a Statement"
  return s

-- | parses a 'source' header, returning the language
aSource :: Parser Statement 
aSource = do
  Tok.keyword "source"
  lang <- many1 Tok.nonSpace
  Tok.chop
  source <- many Tok.line
  spaces
  return $ Source lang source

aSignature :: Parser Statement 
aSignature = do
  typename <- Tok.typename
  Tok.keyword "::"
  inputs <- aNameList
  Tok.keyword "->"
  output <- Tok.typename
  constraints <- option [] aConstraintList
  return $ Signature typename inputs output constraints

aConstraintList :: Parser [Constraint]
aConstraintList = do
  Tok.keyword "where"
  xs <- aNameList
  return xs

constraint :: Parser String
constraint = do
  -- TODO - replace with real constraint parser
  s <- many1 alphaNum
  spaces
  return s

aNameList :: Parser [String]
aNameList = do
  x <- constraint
  rs <- many aFollowingName
  return (x:rs)

aFollowingName :: Parser Name
aFollowingName = do
  spaces
  char ','
  spaces
  i <- constraint
  return i
