{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Penny.ListT where

import Control.Applicative
import Control.Monad
import qualified Pipes as P
import Penny.Ledger
import Control.Lens
import Control.Monad.Trans.Class

-- | For the time being this is just a wrapper on 'P.ListT' from
-- pipes.  At some point it might change to something based on the
-- operational package, to allow for improved logging.
newtype ListT m a = ListT (P.ListT m a)
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative)

makeWrapped ''ListT

instance Ledger m => Ledger (ListT m) where
  newtype PriceL (ListT m) = PriceListT (PriceL m)
  newtype TransactionL (ListT m) = TransactionListT (TransactionL m)
  newtype TreeL (ListT m) = TreeListT (TreeL m)
  newtype PostingL (ListT m) = PostingListT (PostingL m)

  vault = ListT $ lift Penny.Ledger.vault
