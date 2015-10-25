{-# LANGUAGE TemplateHaskell #-}
module Penny.Scalar where


import Control.Lens
import Data.Text (Text)
import Data.Time

data Scalar
  = SText Text
  | SDay Day
  | STime TimeOfDay
  | SZone Int
  | SInteger Integer
  deriving (Eq, Ord, Show)

makePrisms ''Scalar
