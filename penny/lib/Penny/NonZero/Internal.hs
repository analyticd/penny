{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.NonZero.Internal where

import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

newtype NonZero = NonZero { c'Integer'NonZero :: Integer }
  deriving (Eq, Ord, Show)

instance PrettyVal NonZero where
  prettyVal (NonZero i) = Pretty.Con "Penny.NonZero"
    [Pretty.Integer . show $ i]

c'NonZero'Integer :: Integer -> Maybe NonZero
c'NonZero'Integer i
  | i == 0 = Nothing
  | otherwise = Just $ NonZero i
