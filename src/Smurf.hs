-- This is test compiler for Morloc typestrings 

module Smurf (parseSmurf) where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

type IParser a = IndentParser String () a
type Smurf = [NamedList]
data NamedList = NamedList Name [ItemList] deriving (Show)
type Name = String
type Item = String
type ItemList = [Item]

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndentParser aParser () source_name input

parseSmurf :: SourceName -> String -> String
parseSmurf file s =
  case iParse smurf file s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"

smurf :: IParser Smurf
smurf = many1 aNamedList

aNamedList :: IParser NamedList
aNamedList = do
  b <- withBlock NamedList aName anItemList
  spaces
  return b

aName :: IParser Name
aName = do
  s <- many1 alphaNum
  _ <- char ':'
  spaces
  return s

anItemList :: IParser ItemList
anItemList = do
  x <- anItem
  rs <- many aFollowingItem
  return (x:rs)

aFollowingItem :: IParser Item
aFollowingItem = do
  spaces
  char ','
  spaces
  i <- anItem
  return i

anItem :: IParser Item
anItem = do
  i <- many1 alphaNum
  spaces
  return i
