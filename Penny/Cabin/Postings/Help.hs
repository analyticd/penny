module Penny.Cabin.Postings.Help where

import qualified Data.Text as X

helpStr :: String
helpStr = unlines [
  "postings, pos",
  "  Show postings in order with a running balance.",
  "  The postings report takes the same options shown",
  "  above; however, these options affect which postings",
  "  are shown in the report. Postings not shown still affect",
  "  the running balance.",
  "",
  "  Additional filtering options:"
