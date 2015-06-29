{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Transbox where

import Control.Lens
import Penny.Ledger
import Data.Sequence (Seq)

data Transbox a = Transbox
  { _transaction :: TransactionL
  , _transboxee :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Transbox

txnMeta :: Transbox a -> Ledger (Seq TreeL)
txnMeta = Penny.Ledger.txnMeta . view transaction

