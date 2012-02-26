module Penny.Cabin.Postings.Help where

import qualified Data.Text as X

helpStr :: String
helpStr = unlines [
  "postings, pos",
  "  Show postings in order with a running balance.",
  "  The postings report takes the same options shown",
  "  from above in the categories \"Posting filters\" to",
  "  \"Removing postings after sorting and filtering\",",
  "  with the options affecting which postings are shown on screen.",
  "  Postings not shown still affect the running balance.",
  "",
  "  Additional options:",
  "",
  "  --color yes|no|auto|256",
  "    yes: show 8 colors always",
  "    no: never show colors",
  "    auto: show 8 or 256 colors, but only if stdout is a terminal",
  "    256: show 256 colors always",
  "",
  "  --width num",
  "    Hint for roughly how wide the report should be in columns",
  "  --show field, --hide field",
  "    show or hide this field, where field is one of:",
  "      linenum, date, flag, number, payee, account,",
  "      postingDrCr, postingCommodity, postingQty,",
  "      totalDrCr, totalCommodity, totalQty,",
  "      tags, memo, filename"
  ]
