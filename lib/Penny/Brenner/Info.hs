{-# LANGUAGE OverloadedStrings #-}
module Penny.Brenner.Info (mode) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Brenner.Types as Y
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import Data.Monoid ((<>))
import qualified Penny.Lincoln as L
import qualified Penny.Copper.Render as R
import qualified System.Console.MultiArg as MA

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " info [options]"
  , "Shows further information about the configuration of your"
  , "financial institution accounts."
  , ""
  , "Options:"
  , "  -h, --help - show help and exit"
  ]

mode :: Y.Config -> Y.Mode
mode = MA.modeHelp
  "info"               -- Mode name
  help                 -- Help function
  (\_ _ -> process cf) -- Processing function
  []                   -- Options
  MA.Intersperse       -- Interspersion
  processPa            -- Posarg processor
  where
    processPa = const . Ex.throw . MA.ErrorMsg
      $ "this mode does not accept positional arguments"

process :: Y.Config -> IO ()
process cn cf = TIO.putStr $ showInfo cf cn

showInfo :: Y.Config -> X.Text
showInfo cf =
  "These settings are compiled into your program.\n\n"
  <> showConfig cf

showConfig :: Y.Config -> X.Text
showConfig (Y.Config dflt more) =
  "Default financial institution account:"
  <> case dflt of
      Nothing -> " (no default)\n\n"
      Just d -> "\n\n" <> showFitAcct d <> "\n"
  <> "Additional financial institution accounts:"
  <> case more of
      [] -> " no additional accounts\n"
      ls -> "\n\n" <> showFitAccts ls

sepBar :: X.Text
sepBar = X.replicate 40 "=" <> "\n"

sepWithSpace :: X.Text
sepWithSpace = "\n" <> sepBar <> "\n"

showFitAccts :: [Y.FitAcct] -> X.Text
showFitAccts = X.intercalate sepWithSpace . map showFitAcct

label :: X.Text -> X.Text -> X.Text
label l t = l <> ": " <> t

showFitAcct :: Y.FitAcct -> X.Text
showFitAcct c =
  (L.text . Y.fitAcctName $ c) <> "\n\n"
  <> (L.text . Y.fitAcctDesc $ c) <> "\n"
  <> X.unlines
  [ label "Database location" (L.text . Y.dbLocation $ c)
  , label "Penny account" (L.text . L.Delimited ":" . Y.pennyAcct $ c)
  , label "Default account" (L.text . L.Delimited ":" . Y.defaultAcct $ c)
  , label "Currency" (L.text . Y.currency $ c)
  , label "Group amounts to left of decimal point"
    (showGroupLeft . R.left . Y.groupSpecs $ c)

  , label "Group amounts to right of decimal point"
    (showGroupRight . R.right . Y.groupSpecs $ c)

  , label "Financial institution increases are"
    (showTranslator . Y.translator $ c)

  , label "In new postings, commodity is on the"
    (showSide . Y.side $ c)

  , label "Space between commodity and quantity in new postings"
    (showSpaceBetween . Y.spaceBetween $ c)
  ]
  <> "Parser description:\n"
  <> (L.text . fst . Y.parser $ c)

showGroupLeft :: R.GroupSpec -> X.Text
showGroupLeft s = case s of
  R.NoGrouping -> "never"
  R.GroupLarge -> "when greater than 9,999"
  R.GroupAll -> "always"

showGroupRight :: R.GroupSpec -> X.Text
showGroupRight s = case s of
  R.NoGrouping -> "never"
  R.GroupLarge -> "when mor than four decimal places"
  R.GroupAll -> "always"

showTranslator :: Y.Translator -> X.Text
showTranslator y = case y of
  Y.IncreaseIsDebit -> "debits"
  Y.IncreaseIsCredit -> "credits"

showSide :: L.Side -> X.Text
showSide L.CommodityOnLeft = "left"
showSide L.CommodityOnRight = "right"

showSpaceBetween :: L.SpaceBetween -> X.Text
showSpaceBetween L.SpaceBetween = "yes"
showSpaceBetween L.NoSpaceBetween = "no"

{-
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
