module Penny.NonNegative.Internal where

import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

newtype NonNegative = NonNegative { c'Integer'NonNegative :: Integer }
  deriving (Eq, Ord, Show)

instance PrettyVal NonNegative where
  prettyVal (NonNegative i) = Pretty.Con "Penny.NonNegative"
    [Pretty.Integer (show i)]
