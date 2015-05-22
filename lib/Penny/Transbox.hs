{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Transbox where

import Control.Lens
import Penny.Ledger
import Data.Foldable
import Data.Sequence (Seq)

data Transbox l a = Transbox
  { _transaction :: TransactionL l
  , _transboxee :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Transbox

txnMeta :: Ledger l => Transbox l a -> l (Seq (TreeL l))
txnMeta = Penny.Ledger.txnMeta . view transaction

