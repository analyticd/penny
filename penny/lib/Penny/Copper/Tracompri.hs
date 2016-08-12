{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Copper.Tracompri where

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

