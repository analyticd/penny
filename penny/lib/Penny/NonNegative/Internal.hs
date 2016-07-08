{-# LANGUAGE DeriveDataTypeable #-}
module Penny.NonNegative.Internal where

import Data.Data (Data)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

-- | Values that must not be negative.  (They can be zero.)
newtype NonNegative = NonNegative { c'Integer'NonNegative :: Integer }
  deriving (Eq, Ord, Show, Data)

instance PrettyVal NonNegative where
  prettyVal (NonNegative i) = Pretty.Con "Penny.NonNegative"
    [Pretty.Integer (show i)]
