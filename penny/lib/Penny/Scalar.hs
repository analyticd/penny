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
  | SLabel Text
  -- ^ The idea is that a 'SText' is a piece of data, while
  -- an 'SLabel' is a label and is not itself data.
  -- Uses for this might be @SLabel "flag"@ or
  -- @SLabel "fitid"@ for example.
  deriving (Eq, Ord, Show)

makePrisms ''Scalar

instance Display Scalar where
  display x = case x of
    SText a -> display a
    SDay a -> display a
    STime a -> display a
    SZone a -> display . minutesToTimeZone $ a
    SInteger a -> display a
    SLabel a -> ('\'':) . display a

