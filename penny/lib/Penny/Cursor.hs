{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Locations of items.

module Penny.Cursor where

import qualified Control.Lens as Lens
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty
import qualified Pinchot

import Penny.Pretty

-- | Indicates where this input file came from.
data InputFilespec
  = Stdin
  | GivenFilename Text
  deriving (Eq, Show, Ord, Generic)

instance PrettyVal InputFilespec where
  prettyVal Stdin = Pretty.Con "Stdin" []
  prettyVal (GivenFilename txt) = Pretty.Con "GivenFilename" [prettyText txt]

data Cursor = Cursor
  { _filename :: InputFilespec
  , _loc :: Pinchot.Loc
  } deriving (Eq, Ord, Show, Generic, PrettyVal)

Lens.makeLenses ''Cursor
