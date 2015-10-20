#!/usr/bin/env stack
-- stack --resolver lts-3.10 --install-ghc runghc

import Data.Char (toUpper)

makeFunction :: String -> String
makeFunction nm = unlines
  [ nm ++ " :: Counter " ++ typeNm
  , nm ++ " = nextChar"
  , ""
  ]
  where
    typeNm = case nm of
      [] -> error "empty name"
      x:xs -> toUpper x : xs

main :: IO ()
main = interact $ concatMap makeFunction . lines
