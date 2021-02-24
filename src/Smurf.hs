module Smurf (parseSmurf) where

import Smurf.Data
import Smurf.Parser
import Text.Megaparsec

parseSmurf :: String -> String -> String
parseSmurf f s =
    case runParser smurf f s of
        Left e -> errorBundlePretty e
        Right v -> show v

-- | a somewhat pretty printer, it doesn't convert all the way back to the
-- input code, although perhaps that would be a reasonable thing to do. 
showSmurf :: [Top] -> String
showSmurf = unlines . map showSmurf' where
  showSmurf' :: Top -> String
 -- showSmurf' (TopImport (Import path qual rest)) =
  --   unwords ["from", show path, "import", show rest, "as", show qual]
  showSmurf' (TopStatement (Signature name ins out preds)) =
    unwords [name, "::", show ins, "->", show out, "where", show preds]  
--  showSmurf' (TopSource x) = ""
  showSmurf' x = show x
