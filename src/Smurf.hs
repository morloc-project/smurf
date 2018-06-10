-- This is test compiler for Morloc typestrings 

module Smurf (parseSmurf) where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

parseSmurf :: SourceName -> String -> String
parseSmurf file s =
  case iParse aNamedList file s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"

type IParser a = IndentParser String () a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndentParser aParser () source_name input

data NamedList = NamedList Name [Item] deriving (Show)

type Name = String
type Item = String

aNamedList :: IParser NamedList
aNamedList = do
  b <- withBlock NamedList aName anItem
  spaces
  return b

aName :: IParser Name
aName = do
  s <- many1 alphaNum
  _ <- char ':'
  spaces
  return s

anItem :: IParser Item
anItem = do
  i <- many1 alphaNum
  spaces
  return i
