{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
-- | Productions that have more than one possible value.  A sensible
-- default value is used.

module Penny.Copper.Formatted where

import Penny.Copper.Char
import Penny.Copper.Optics
import Penny.Copper.Types
import Penny.Copper.Singleton
import Penny.NonNegative (NonNegative)
import qualified Penny.NonNegative as NonNegative
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Positive

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X
import qualified Control.Lens as Lens
import Pinchot (NonEmpty(NonEmpty))

