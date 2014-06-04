module Penny.Lincoln.Natural.Generators where

import Test.QuickCheck
import qualified Penny.Lincoln.Natural as N

nonNegative :: Gen N.NonNegative
nonNegative = do
  i <- fmap abs arbitrarySizedIntegral
  case N.nonNegative i of
    Nothing -> error "failed to generate NonNegative"
    Just n -> return n

positive :: Gen N.Positive
positive = do
  i <- fmap abs $ arbitrarySizedIntegral `suchThat` (/= 0)
  case N.positive i of
    Nothing -> error "failed to generate Positive"
    Just n -> return n
