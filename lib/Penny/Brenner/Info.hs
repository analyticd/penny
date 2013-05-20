module Penny.Brenner.Info (mode) where

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " info [options]"
  , "Shows further information about the configuration of your"
  , "financial institution accounts."
  , ""
  , "Options:"
  , "  -h, --help - show help and exit"
  ]
