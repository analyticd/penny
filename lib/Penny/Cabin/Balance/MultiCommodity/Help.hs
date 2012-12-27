module Penny.Cabin.Balance.MultiCommodity.Help where

import qualified Data.Text as X

help :: X.Text
help = X.pack . unlines $ [
  "balance, bal",
  "  Show account balances. Accepts ONLY the following options:",
  "",
  "  --show-zero-balances",
  "    Show balances that are zero (default)",
  "  --hide-zero-balances",
  "    Hide balances that are zero",
  "",
  "  --ascending",
  "    Sort in ascending order by account name (default)",
  "  --descending",
  "    Sort in descending order by account name)",
  ""
  ]

