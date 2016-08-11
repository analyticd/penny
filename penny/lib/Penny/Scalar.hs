{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Scalar where


import Control.Lens
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

import Penny.Pretty

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
  deriving (Eq, Ord, Show, Generic)

instance PrettyVal Scalar where
  prettyVal a = case a of
    SText x -> Pretty.Con "SText" [prettyText x]
    SDay x -> Pretty.Con "SDay" [prettyDay x]
    STime x -> Pretty.Con "STime" [prettyTimeOfDay x]
    SZone x -> Pretty.Con "SZone" [Pretty.prettyVal x]
    SInteger x -> Pretty.Con "SInteger" [Pretty.prettyVal x]
    SLabel x -> Pretty.Con "SLabel" [prettyText x]

makePrisms ''Scalar

displayScalar :: Scalar -> Text
displayScalar x = case x of
  SText a -> a
  SDay a -> X.pack . show $ a
  STime a -> X.pack . show $ a
  SZone a -> X.pack . show . minutesToTimeZone $ a
  SInteger a -> X.pack . show $ a
  SLabel a -> X.cons '\'' a

