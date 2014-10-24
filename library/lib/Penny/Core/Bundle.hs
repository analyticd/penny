module Penny.Core.Bundle
  ( T
  , up
  , down
  , toTransaction
  , topLine
  , postings
  ) where

import Penny.Core.Bundle.Internal
import qualified Penny.Core.Transaction as Transaction
import qualified Penny.Core.Balanced.Internal as Balanced.Internal
import Data.Sequence ((|>))
import qualified Penny.Core.View as View
import Data.Monoid

up :: T -> Maybe T
up (T t v) = fmap (T t) . View.moveLeft $ v

down :: T -> Maybe T
down (T t v) = fmap (T t) . View.moveRight $ v

toTransaction :: T -> Transaction.T
toTransaction (T t (View.T l c r))
  = Transaction.T t (Balanced.Internal.T ((l |> c) <> r))
