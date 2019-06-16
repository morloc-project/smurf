module Smurf (parseSmurf) where

import System.IO
import System.IO.Error
import qualified Smurf.Lexer as L
import qualified Smurf.Parser as P
import qualified Smurf.AST as S
import qualified Smurf.Evaluator as E
import Codec.Binary.UTF8.String (encode)

parseSmurf :: String -> String
parseSmurf s = case L.evalP P.parse (encode s) of
  Right result -> show result ++ "\n"
  Left err -> error err
