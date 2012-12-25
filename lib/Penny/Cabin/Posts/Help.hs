module Penny.Cabin.Posts.Help where

import qualified Data.Text as X

help :: X.Text
help = X.pack helpStr

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
  "  Additional sequence number filtering options",
  "    Like the \"Sequence numbers\" options above, these",
  "    options take the form --option cmp num.",
  "",
  "    --filtered, --revFiltered",
  "      All postings, after filters given in the filter",
  "      specification portion of the command line are",
  "      applied",
  "    --sorted, --revSorted",
  "      All postings remaining after filtering and after",
  "      postings have been sorted",
  "",
  "  Other additional options:",
  "",
  "  --width num",
  "    Hint for roughly how wide the report should be in columns",
  "  --show field, --hide field",
  "    show or hide this field, where field is one of:",
  "      globalTransaction, revGlobalTransaction,",
  "      globalPosting, revGlobalPosting,",
  "      fileTransaction, revFileTransaction,",
  "      filePosting, revFilePosting,",
  "      filtered, revFiltered,",
  "      sorted, revSorted,",
  "      visible, revVisible,",
  "      lineNum,",
  "      date, flag, number, payee, account,",
  "      postingDrCr, postingCommodity, postingQty,",
  "      totalDrCr, totalCommodity, totalQty,",
  "      tags, memo, filename",
  "  --show-all",
  "    Show all fields",
  "  --hide-all",
  "    Hide all fields",
  "",
  "  --show-zero-balances",
  "    Show balances that are zero",
  "  --hide-zero-balances",
  "    Hide balances that are zero",
  ""
  ]
