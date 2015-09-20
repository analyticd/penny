{-# LANGUAGE TemplateHaskell #-}
module Penny.Scalar where


import Control.Lens
import Data.Text (Text)
import Data.Time
import Penny.Display

data Scalar
  = SText Text
  | SDay Day
  | STime TimeOfDay
  | SZone Int
  | SInteger Integer
  deriving (Eq, Ord, Show)

makePrisms ''Scalar

instance Display Scalar where
  display x = case x of
    SText a -> display a
    SDay a -> display a
    STime a -> display a
    SZone a -> display . minutesToTimeZone $ a
    SInteger a -> display a

