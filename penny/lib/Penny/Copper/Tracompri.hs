{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
-- | A sum type for transactions, comments, and prices.
module Penny.Copper.Tracompri where

import qualified Control.Lens as Lens
import Data.Text (Text)

import Penny.Price
import Penny.Transaction

-- | A sum type to represent a 'Transaction', a comment, or a
-- price.
data Tracompri a
  = Tracompri'Transaction (Transaction a)
  | Tracompri'Comment Text
  | Tracompri'Price (Price a)
  deriving (Functor, Foldable, Traversable)

Lens.makePrisms ''Tracompri
