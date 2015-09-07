{-# LANGUAGE TemplateHaskell #-}
module Penny.Scalar where


import Control.Lens
import Data.Text (Text)
import Penny.DateTime
import Penny.Display

data Scalar
  = SText Text
  | SDate Date
  | STime Time
  | SZone Zone
  | SInteger Integer
  deriving (Eq, Ord, Show)

makePrisms ''Scalar

instance Display Scalar where
  display x = case x of
    SText a -> display a
    SDate a -> display a
    STime a -> display a
    SZone a -> display a
    SInteger a -> display a

