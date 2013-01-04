module Penny.Cabin.Balance.MultiCommodity.Help where

help :: String
help = unlines [
  "balance",
  "  Show account balances. Accepts ONLY the following options:",
  "",
  "  --help, -h",
  "    Show this help and exit",
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

