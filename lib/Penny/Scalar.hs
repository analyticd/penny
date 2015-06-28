{-# LANGUAGE TemplateHaskell #-}
module Penny.Scalar where


import Control.Lens
import Data.Text (Text, pack)
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

displayScalar :: Scalar -> Text
displayScalar sc = case sc of
  SText x -> x
  SDate d -> pack . display d $ ""
  STime ti -> pack . display ti $ ""
  SZone zn -> pack . display zn $ ""
  SInteger i -> pack . display i $ ""
