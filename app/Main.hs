module Main where

import Lib
import Types
import Parser

main :: IO ()
main = do
    score <- parse "sample_score/dive_air04FD.bme"
    print $ header score
