{-# LANGUAGE FlexibleContexts #-}

module Smurf (parseSmurf) where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

type IParser a = IndentParser String () a
type Smurf = [Statement]
data Statement
  = Signature Name [InputType] OutputType [Constraint]
  | Source Name [String]
  deriving(Show, Ord, Eq)

type InputType  = String
type OutputType = String
type Constraint = String
type Name = String

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndentParser aParser () source_name input

parseSmurf :: SourceName -> String -> String
parseSmurf file s =
  case iParse smurf file s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"

smurf :: IParser Smurf
smurf = do
  result <- many1 aStatement 
  -- eof
  return result

aStatement :: IParser Statement
aStatement = do
  s <-  try aSignature
    <|> try aSource
    <?> "a Statement"
  return s

aSource :: IParser Statement
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

aSpace :: IParser Char
aSpace = char ' ' <|> char '\t'

aSourceLine :: IParser String
aSourceLine = do
  s <- many1 aSpace
  l <- many (noneOf "\n")
  newline
  return $ (s ++ l)

aSignature :: IParser Statement 
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

aConstraintList :: IParser [Constraint]
aConstraintList = do
  string "where"
  spaces
  xs <- aNameList
  return xs

aName :: IParser Name
aName = do
  s <- many1 alphaNum
  spaces
  return s

aNameList :: IParser [String]
aNameList = do
  x <- aName
  rs <- many aFollowingName
  return (x:rs)

aFollowingName :: IParser Name
aFollowingName = do
  spaces
  char ','
  spaces
  i <- aName
  return i
