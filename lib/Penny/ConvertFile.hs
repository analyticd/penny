{-# LANGUAGE BangPatterns #-}
module Penny.ConvertFile where

import qualified Penny.Grammar as G
import qualified Data.Sequence as Seq
import Data.Sequence (viewr, ViewR(EmptyR, (:>)))

