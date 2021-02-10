module Main where

import Smurf 

main :: IO ()
main = interact (parseSmurf "stdin")

