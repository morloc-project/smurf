module Smurf (parseSmurf) where

import Smurf.Data
import Smurf.Parser
import Text.Parsec (parse, SourceName)

parseSmurf :: SourceName -> String -> String
parseSmurf file s =
  case parse smurf file s of
    Left  err    -> show err    ++ "\n"
    Right result -> show result ++ "\n"
