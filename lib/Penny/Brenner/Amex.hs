-- | Amex - processing American Express downloaded data
module Penny.Brenner.Amex where

import qualified Penny.Brenner.Amex.Types as Y
import qualified Data.Text as X
import qualified Penny.Lincoln as L

help ::
  String
  -- ^ Program name

  -> Y.Config
  -> String
help n c = unlines ls ++ cs
  where
    ls = [ "usage: " ++ n ++ " [options] import|merge|clear ARGS..."
         , ""
         , "For help on an individual command, use"
         , n ++ "COMMAND --help"
         , "Options:"
         , "-c, --card CARD"
         , "  Use one of the cards shown below. If this option"
         , "  does not appear, the default card is used if there"
         , "  is one."
         , "-h, --help"
         , "  Show help and exit"
         ]
    showPair (Y.Name a, cd) = showCard a cd
    cs = showDefaultCard (Y.defaultCard c)
         ++ (concatMap showPair . Y.moreCards $ c)

showDefaultCard :: Maybe Y.Card -> String
showDefaultCard mc = case mc of
  Nothing -> "No default card\n"
  Just c -> showCard "<DEFAULT>" c

label :: String -> String -> String
label l o = "  " ++ l ++ ": " ++ o ++ "\n"

showAccount :: L.Account -> String
showAccount =
  X.unpack
  . X.intercalate (X.singleton ':')
  . map L.unSubAccount
  . L.unAccount

showCard :: String -> Y.Card -> String
showCard n c =
  "Card " ++ n ++ ":\n"
  ++ label "Database location" (Y.unDbLocation . Y.dbLocation $ c)

  ++ label "Amex ledger account"
     (showAccount . Y.unAmexAcct . Y.amexAcct $ c)

  ++ label "Account for new offsetting postings"
     (showAccount . Y.unDefaultAcct . Y.defaultAcct $ c)

  ++ label "Currency"
     (X.unpack . L.unCommodity . Y.unCurrency . Y.currency $ c)

