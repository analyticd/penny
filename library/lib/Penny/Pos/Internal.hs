{-# OPTIONS_HADDOCK not-home #-}
module Penny.Pos.Internal where

newtype T = T { toInteger :: Integer }
  deriving (Eq, Ord, Show)

