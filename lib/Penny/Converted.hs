{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Converted where

import Penny.Amount
import qualified Data.Traversable as T
import qualified Data.Foldable as F

data Converted a = Converted (Maybe Amount) a
  deriving (Functor, T.Traversable, F.Foldable)
