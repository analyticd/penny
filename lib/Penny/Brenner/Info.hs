{-# LANGUAGE OverloadedStrings #-}
module Penny.Brenner.Info (mode) where

import qualified Penny.Brenner.Types as Y
import qualified Data.Text as X

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " info [options]"
  , "Shows further information about the configuration of your"
  , "financial institution accounts."
  , ""
  , "Options:"
  , "  -h, --help - show help and exit"
  ]

mode = undefined

{-
showFitAcct :: (Y.Name, Y.FitAcct) -> String
showFitAcct (name, a) = X.unlines
  [ Y.unName name
  , ""
  , 

showFitAcct :: Y.FitAcct -> String
showFitAcct c =
  label "Database location"
    (X.unpack . Y.unDbLocation . Y.dbLocation $ c)

  ++ label "Penny account"
     (showAccount . Y.unPennyAcct . Y.pennyAcct $ c)

  ++ label "Account for new offsetting postings"
     (showAccount . Y.unDefaultAcct . Y.defaultAcct $ c)

  ++ label "Currency"
     (X.unpack . L.unCommodity . Y.unCurrency . Y.currency $ c)

  ++ "\n"

  ++ "More information about the parser:\n"
  ++ (Y.unParserDesc . fst . Y.parser $ c)
  ++ "\n\n"


-}
