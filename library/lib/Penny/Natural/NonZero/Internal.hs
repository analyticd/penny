{-# OPTIONS_HADDOCK not-home #-}
module Penny.Natural.NonZero.Internal where

newtype T = T { toInteger :: Integer }
  deriving (Eq, Ord, Show)

