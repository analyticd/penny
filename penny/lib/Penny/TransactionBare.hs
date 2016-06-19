{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | A bare transaction; that is, one without any serial numbers.
module Penny.TransactionBare where

import Penny.Ents
import Penny.Tranche

import qualified Control.Lens as Lens

data TransactionBare a = TransactionBare
  { _topLine :: TopLine a
  , _postings :: Balanced (Postline a)
  } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''TransactionBare

