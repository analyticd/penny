{-# LANGUAGE TemplateHaskell #-}
module Penny.Scalar where


import Control.Lens
import Data.Text (Text)
import qualified Data.Text as X
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

displayScalar :: Scalar -> Text
displayScalar x = case x of
  SText a -> a
  SDay a -> X.pack . show $ a
  STime a -> X.pack . show $ a
  SZone a -> X.pack . show . minutesToTimeZone $ a
  SInteger a -> X.pack . show $ a
  SLabel a -> X.cons '\'' a

