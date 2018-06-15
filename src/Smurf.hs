module Smurf (parseSmurf) where

import Smurf.Data
import Smurf.Parser
import Text.Parsec (parse, SourceName)

parseSmurf :: SourceName -> String -> String
parseSmurf file s =
  case parse smurf file s of
    Left  err    -> show      err    ++ "\n"
    Right result -> showSmurf result ++ "\n"

-- | a somewhat pretty printer, it doesn't convert all the way back to the
-- input code, although perhaps that would be a reasonable thing to do. 
showSmurf :: [Top] -> String
showSmurf = (unlines . map showSmurf') where
  showSmurf' :: Top -> String
  showSmurf' (TopImport (Import path qual rest)) =
    unwords ["from", show path, "import", show rest, "as", show qual]
  showSmurf' (TopStatement (Signature name ins out constraints)) =
    unwords [name, "::", show ins, "->", show out, "where", show constraints]  
  showSmurf' (TopSource x) = ""
  showSmurf' x = show x
