{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

parens :: _ -> Parser a
parens = between (char '(') (char ')')

aStatement :: Parser Statement
aStatement = do
  s <-  try aSignature
    <|> try aSource
    <|> try aRestrictedImport
    <|> try aSimpleImport
    <?> "a Statement"
  return s

aSimpleImport :: Parser Statement
aSimpleImport = do
  Tok.keyword "import" 
  path <- Tok.path
  spaces
  qual <- optionMaybe (Tok.keyword "as" >> Tok.pathElement)
  return $ Import (Package path qual Nothing)

aRestrictedImport :: Parser Statement
aRestrictedImport = do
  Tok.keyword "from"
  path <- Tok.path
  Tok.keyword "import"
  functions <- parens (sepBy1 Tok.name Tok.comma)
  spaces
  return $ Import (Package path Nothing (Just functions))

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
