-- | Denver - Penny compatibility with John Wiegley's Ledger written
-- in C++. WARNING - I wrote this module so that I could convert my
-- data from the Ledger 2.6 series to Penny. I did not use all of
-- Ledger's features, so this code does not handle all Ledger
-- features.
module Penny.Denver (ledger) where

import Penny.Denver.Parser (ledger)
