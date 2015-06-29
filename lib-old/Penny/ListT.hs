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
import Control.Monad.IO.Class
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | For the time being this is just a wrapper on 'P.ListT' from
-- pipes.  At some point it might change to something based on the
-- operational package, to allow for improved logging.
newtype ListT m a = ListT (P.ListT m a)
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative, Monoid)

makeWrapped ''ListT

instance MonadIO m => MonadIO (ListT m) where
  liftIO = ListT . liftIO

observe :: Monad m => ListT m a -> m (Maybe (a, ListT m a))
observe (ListT (P.Select pdcr)) = liftM eval . P.next $ pdcr
  where
    eval (Left ()) = Nothing
    eval (Right (a, pdcr')) = Just (a, ListT (P.Select pdcr'))

toBool :: Monad m => ListT m a -> m Bool
toBool = liftM (maybe False (const True)) . observe

observeAll
  :: Monad m
  => ListT m a
  -> m (Seq a)
observeAll list = do
  mayR <- observe list
  case mayR of
    Nothing -> return Seq.empty
    Just (x, xs) -> do
      rest <- observeAll xs
      return $ x <| rest
