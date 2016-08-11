module Penny.Copper.Tracompri where

import Data.Text (Text)

import Penny.Copper.PriceParts
import Penny.TransactionBare

-- | A sum type to represent a 'TransactionBare', a comment, or a
-- price.
data Tracompri a
  = Tracompri'Transaction (TransactionBare a)
  | Tracompri'Comment Text
  | Tracompri'Price (PriceParts a)

