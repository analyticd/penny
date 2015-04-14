{-# LANGUAGE OverloadedStrings #-}
-- | The Register report
module Penny.Register
  ( BestField
  , original
  , best
  , balance

  , makeRegisterReport
  ) where

import Penny.Amount
import Penny.Clatch
import Penny.Representation
import Penny.Ledger
import Rainbox
import Penny.Register.Individual
  ( LineTag, BestField )
import qualified Penny.Register.Individual as I
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Text (Text)

type Column l
  = (Amount -> RepNonNeutralNoSide)
  -> Clatch l
  -> l (Seq (LineTag, Text))

newtype Columns l = Columns (Seq (Column l))

instance Monoid (Columns l) where
  mempty = Columns Seq.empty
  mappend (Columns x) (Columns y) = Columns $ x <> y

original
  :: BestField l (Amount -> RepNonNeutralNoSide)
  -> Columns l
original = Columns . Seq.singleton . I.original

best
  :: BestField l (Amount -> RepNonNeutralNoSide)
  -> Columns l
best = Columns . Seq.singleton . I.best

balance
  :: BestField l (Amount -> RepNonNeutralNoSide)
  -> Columns l
balance = Columns . Seq.singleton . I.balance


makeRegisterReport
  :: Ledger l
  => (Amount -> RepNonNeutralNoSide)
  -- ^ How to format an 'Amount' that is not already represented.
  -> Columns l
  -> l (Box Vertical)
makeRegisterReport = undefined
