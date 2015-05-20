{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Transbox where

import Control.Lens
import Penny.Ledger
import Data.Foldable

data Transbox l a = Transbox
  { _transaction :: TransactionL l
  , _transboxee :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Transbox
