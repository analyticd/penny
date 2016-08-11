module Penny.Copper.Tracompri where

import Data.Text (Text)

import Penny.Copper.PriceParts
import Penny.Transaction

-- | A sum type to represent a 'TransactionBare', a comment, or a
-- price.
data Tracompri a
  = Tracompri'Transaction (Transaction a)
  | Tracompri'Comment Text
  | Tracompri'Price (PriceParts a)

