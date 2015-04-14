{-# LANGUAGE OverloadedStrings #-}
-- | The Register report
module Penny.Register where

import Penny.Clatch
import Rainbox
import Penny.Register.Individual
  ( LineTag )
import qualified Penny.Register.Individual as I
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Text (Text)

newtype Columns l = Columns (Seq (Clatch l -> l (Seq (LineTag, Text))))

instance Monoid (Columns l) where
  mempty = Columns Seq.empty
  mappend (Columns x) (Columns y) = Columns $ x <> y

type Column l = Clatch l -> l Cell

makeRegisterReport
  :: [Column l]
  -> Box Vertical
makeRegisterReport = undefined
