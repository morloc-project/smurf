{

{-# LANGUAGE FlexibleContexts #-}

module Smurf.Parser(parse) where
import Smurf.AST
import Smurf.Lexer
import Control.Monad.Error
}

%monad{P}
%lexer{lexer}{TEOF}
%name parse
%tokentype{Token}
%error{parseError}

%token
  true   {TTrue}
  false  {TFalse}
  zero   {TZero}
  iszero {TIsZero}
  succ   {TSucc}
  pred   {TPred}
  if     {TIf}
  then   {TThen}
  else   {TElse}

%%

Term
  :  true   {STrue}
  |  false  {SFalse}
  |  zero   {SZero}
  |  iszero Term {SIsZero $2}
  |  succ   Term {SSucc   $2}
  |  pred   Term {SPred   $2}
  |  if     Term then Term else Term {SIfThen $2 $4 $6}

{
parseError _ = throwError "!Parse Error"
}
