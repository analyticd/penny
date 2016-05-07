{-# LANGUAGE FlexibleContexts #-}
-- | Converting characters and strings to Copper types.  Many of
-- these functions can fail.
module Penny.Copper.Char where

import Penny.Copper.Optics
import Penny.Copper.Singleton
import Penny.Copper.Types
import Penny.SeqUtil (convertHead)

import Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Pinchot

