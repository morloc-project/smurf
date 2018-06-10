{-# LANGUAGE FlexibleContexts #-}

module Smurf (parseSmurf) where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Control.Monad.State

type Smurf = [Statement]
data Statement
  = Signature Name [InputType] OutputType [Constraint]
  | Source Name [String]
  deriving(Show, Ord, Eq)

type InputType  = String
type OutputType = String
type Constraint = String
type Name = String

parseSmurf :: SourceName -> String -> String
parseSmurf file s =
  case parse smurf file s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"

smurf :: Parser Smurf
smurf = do
  result <- many1 aStatement 
  -- eof
  return result

aStatement :: Parser Statement
aStatement = do
  s <-  try aSignature
    <|> try aSource
    <?> "a Statement"
  return s

aSource :: Parser Statement
aSource = do
  spaces
  string "source"
  many aSpace
  lang <- aName
  char ':'
  many aSpace
  char '\n'
  source <- many aSourceLine
  return $ Source lang source

aSpace :: Parser Char
aSpace = char ' ' <|> char '\t'

aSourceLine :: Parser String
aSourceLine = do
  s <- many1 aSpace
  l <- many (noneOf "\n")
  newline
  return $ (s ++ l)

aSignature :: Parser Statement 
aSignature = do
  typename <- aName
  string "::"
  spaces
  inputs <- aNameList
  string "->"
  spaces
  output <- aName
  constraints <- option [] aConstraintList
  return $ Signature typename inputs output constraints

aConstraintList :: Parser [Constraint]
aConstraintList = do
  string "where"
  spaces
  xs <- aNameList
  return xs

aName :: Parser Name
aName = do
  s <- many1 alphaNum
  spaces
  return s

aNameList :: Parser [String]
aNameList = do
  x <- aName
  rs <- many aFollowingName
  return (x:rs)

aFollowingName :: Parser Name
aFollowingName = do
  spaces
  char ','
  spaces
  i <- aName
  return i
