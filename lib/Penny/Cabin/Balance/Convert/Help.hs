module Penny.Cabin.Balance.Convert.Help where

import qualified Data.Text as X

help :: X.Text
help = X.pack . unlines $ [
  "convert",
  "  Show account balances, after converting all amounts",
  "  to a single commodity. Accepts ONLY the following options:",
  "",
  "    --color yes|no|auto|256",
  "    yes: show 8 colors always",
  "    no: never show colors (default)",
  "    auto: show 8 or 256 colors, but only if stdout is a terminal",
  "    256: show 256 colors always",
  "  --background light|dark",
  "    Use appropriate color scheme for terminal background",
  "      (default: dark)",
  "",
  "  --show-zero-balances",
  "    Show balances that are zero",
  "  --hide-zero-balances",
  "    Hide balances that are zero",
  "",
  "--commodity TARGET-COMMMODITY, -c TARGET-COMMODITY",
  "  Convert all commodities to TARGET-COMMODITY. By default,",
  "  the commodity that appears most often as the target commodity",
  "  in your price data is used (if there is a tie, the price closest",
  "  to the end of your list of prices is used)",
  "",
  "--date DATE-TIME, -d DATE-TIME",
  "  Convert prices as of the date and time given",
  "  (by default, the current date and time is used.)",
  "",
  "--sort qty|name, -s qty|name",
  "  Sort balances by sub-account name (default) or by quantity",
  "--ascending",
  "  Sort in ascending order (default)",
  "--descending",
  "  Sort in descending order",
  ""
  ]

