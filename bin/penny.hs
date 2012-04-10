module Main where

import Penny

main :: IO ()
main = zincMain utcDefault periodComma
       (allReportsWithDefaults utcDefault periodComma)
