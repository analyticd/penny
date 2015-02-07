module Main where

mkTest s = "prop_" ++ s ++ " = parseRenderParse p" ++ s

main = do
  types <- fmap lines $ readFile "dateTypes"
  mapM_ (putStrLn . mkTest) types
