module Smurf.Parser (smurf) where

import Text.Parsec hiding (State)
import qualified Text.Parsec.Expr as TPE
import Text.Parsec.String (Parser)
import Control.Monad.State

import Smurf.Data
import qualified Smurf.Lexer as Tok

smurf :: Parser [Top]
smurf = do
  Tok.whiteSpace
  result <- many top
  eof
  return result

top :: Parser Top
top =
      try topSource 
  <|> try topStatement
  <|> try topImport
  <?> "Top. Maybe you are missing a semicolon?"

topSource :: Parser Top
topSource = do
  TopSource <$> source

topStatement :: Parser Top
topStatement = do
  TopStatement <$> statement

topImport :: Parser Top
topImport = do
  i <-  try restrictedImport
    <|> try simpleImport
  return $ TopImport i

statement :: Parser Statement
statement = do
  s <-  try signature
    <|> try declaration
  Tok.op ";"
  return s

simpleImport :: Parser Import
simpleImport = do
  Tok.reserved "import" 
  path <- Tok.path
  qual <- optionMaybe (Tok.op "as" >> Tok.name)
  return $ Import path qual Nothing

restrictedImport :: Parser Import
restrictedImport = do
  Tok.reserved "from"
  path <- Tok.path
  Tok.reserved "import"
  -- TODO: I am also importing ontologies, how should that be handled?
  -- TODO: at very least, I am also importing types
  functions <- Tok.parens (sepBy1 Tok.name Tok.comma)
  return $ Import path Nothing (Just functions)

-- | parses a 'source' header, returning the language
source :: Parser Source
source = do
  Tok.reserved "source"
  lang <- many1 Tok.nonSpace
  Tok.chop
  source <- many Tok.line
  Tok.whiteSpace
  return $ Source lang source

declaration :: Parser Statement
declaration = do
  varname <- Tok.name
  bndvars <- many Tok.name
  Tok.op "="
  Declaration varname bndvars <$> expression

expression :: Parser Expression
expression =
      -- currently this just handles "."
      try (TPE.buildExpressionParser functionTable term')
  <|> term'
  <?> "an expression"
  where
    term' =
          try (Tok.parens expression)
      <|> try application
      <|> try primitiveExpr

primitiveExpr :: Parser Expression
primitiveExpr = do
  ExprPrimitive <$> primitive

primitive :: Parser Primitive
primitive =
      try Tok.floatP -- this must go before integer
  <|> try Tok.integerP
  <|> try Tok.booleanP
  <|> try Tok.stringLiteralP
  <?> "a primitive"

application :: Parser Expression
application = do
  tag' <- Tok.tag Tok.name
  function <- Tok.name
  arguments <- sepBy expression Tok.whiteSpace
  return $ ExprApplication function tag' arguments

-- | function :: [input] -> output constraints 
signature :: Parser Statement
signature = do
  function <- Tok.name
  Tok.op "::"
  inputs <- sepBy1 mtype Tok.comma
  output <- optionMaybe (
      Tok.op "->" >>
      mtype
    )
  constraints <- option [] (
      Tok.reserved "where" >>
      Tok.parens (sepBy1 booleanExpr Tok.comma)
    )
  return $ Signature function inputs output constraints

mtype :: Parser MType
mtype =
      list'       -- [a]
  <|> paren'      -- () | (a) | (a,b,...)
  <|> try record' -- Foo {a :: t, ...}
  <|> specific'   -- Foo
  <|> generic'    -- foo
  <?> "type"
  where
    -- [ <type> ]
    list' :: Parser MType
    list' = do
      l <- Tok.tag (char '[')
      s <- Tok.brackets mtype
      return $ MList s l

    -- ( <type>, <type>, ... )
    paren' :: Parser MType
    paren' = do
      l <- Tok.tag (char '(')
      s <- Tok.parens (sepBy mtype Tok.comma)
      return $ case s of
        []  -> MEmpty
        [x] -> x
        xs  -> MTuple xs l

    -- <name> <type> <type> ...
    specific' :: Parser MType
    specific' = do
      l <- Tok.tag Tok.specificType
      s <- Tok.specificType
      ss <- many mtype 
      return $ MSpecific s ss l

    -- <name> <type> <type> ...
    generic' :: Parser MType
    generic' = do
      -- TODO - the genericType should automatically fail on keyword conflict
      notFollowedBy (Tok.reserved "where")
      l <- Tok.tag Tok.genericType
      s <- Tok.genericType
      ss <- many mtype 
      return $ MGeneric s ss l

    -- <name> { <name> :: <type>, <name> :: <type>, ... }
    record' :: Parser MType
    record' = do
      l <- Tok.tag Tok.specificType
      n <- Tok.specificType
      xs <- Tok.braces (sepBy1 recordEntry' Tok.comma)
      return $ MRecord n xs l

    -- (<name> = <type>)
    recordEntry' :: Parser (Name, MType)
    recordEntry' = do
      n <- Tok.name
      Tok.op "::"
      t <- mtype
      return (n, t)


booleanExpr :: Parser BExpr
booleanExpr =
      try booleanBinOp
  <|> try relativeExpr
  <|> try not'
  <|> try (Tok.parens booleanExpr)
  <|> try application'
  <?> "an expression that reduces to True/False"
  where
    not' = do
      Tok.reserved "not"
      NOT <$> booleanExpr

    application' = do
      n <- Tok.name
      ns <- many Tok.name
      return $ BExprFunc n ns

booleanBinOp :: Parser BExpr
booleanBinOp = do
  a <- bterm'
  op <- Tok.logicalBinOp
  binop' op a <$> bterm'
  where
    bterm' = do
      application'
        <|> bool'
        <|> Tok.parens booleanExpr
        <?> "boolean expression"

    application' = do
      n <- Tok.name
      ns <- many Tok.name
      return $ BExprFunc n ns

    bool' = do
      BExprBool <$> Tok.boolean

    binop' op a b
      | op == "and" = AND a b
      | op == "or"  = OR  a b

relativeExpr :: Parser BExpr
relativeExpr = do
  a <- arithmeticExpr
  op <- Tok.relativeBinOp
  relop' op a <$> arithmeticExpr
  where
    relop' op a b
      | op == "==" = EQ' a b
      | op == "!=" = NE' a b
      | op == ">"  = GT' a b
      | op == "<"  = LT' a b
      | op == ">=" = GE' a b
      | op == "<=" = LE' a b

arithmeticExpr
  = TPE.buildExpressionParser arithmeticTable arithmeticTerm
  <?> "expression"

arithmeticTerm
  = do
      Tok.parens arithmeticExpr
  <|> try access'
  <|> val'
  <|> var'
  <?> "simple expression. Currently only integers are allowed"
  where
    val' = do
      toExpr' <$> primitive

    var' = do
      x <- Tok.name
      xs <- option [] (many Tok.name)
      return $ AExprFunc x xs

    access' = do
      x <- Tok.name
      ids <- Tok.brackets (sepBy1 arithmeticExpr Tok.comma)
      return $ AExprAccess x ids

    toExpr' :: Primitive -> AExpr
    toExpr' (PrimitiveInt x) = AExprInt x
    toExpr' (PrimitiveReal x) = AExprReal x
    toExpr' _ = undefined

arithmeticTable
  = [
      [ prefix "-" Neg
      , prefix "+" Pos
      ]             
    , [ binary "^"  Pow TPE.AssocRight
      ]
    , [ binary "*"  Mul TPE.AssocLeft
      , binary "/"  Div TPE.AssocLeft
      , binary "%"  Mod TPE.AssocLeft
      , binary "//" Quo TPE.AssocLeft
      ]
    , [ binary "+"  Add TPE.AssocLeft
      , binary "-"  Sub TPE.AssocLeft
      ]
  ]

functionTable = [[ binary "."  ExprComposition TPE.AssocRight]]

binary name fun = TPE.Infix  (do { Tok.op name; return fun })
prefix name fun = TPE.Prefix (do { Tok.op name; return fun })
