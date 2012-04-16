module Penny.Cabin.Balance.Help where

import qualified Data.Text as X

help :: X.Text
help = X.pack helpStr

helpStr :: String
helpStr = unlines [
  "balance, bal",
  "  Show account balances. Accepts ONLY the following options:",
  "",
  "    --color yes|no|auto|256",
  "    yes: show 8 colors always",
  "    no: never show colors",
  "    auto: show 8 or 256 colors, but only if stdout is a terminal",
  "    256: show 256 colors always",
  "  --background light|dark",
  "    Use appropriate color scheme for terminal background",
  "",
  "  --show-zero-balances",
  "    Show balances that are zero",
  "  --hide-zero-balances",
  "    Hide balances that are zero",
  ""
  ]
  
