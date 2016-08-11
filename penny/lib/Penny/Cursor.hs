{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Locations of items.

module Penny.Cursor where

import qualified Control.Lens as Lens
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty
import qualified Pinchot

import Penny.Pretty

data Cursor = Cursor
  { _filename :: Text
  , _loc :: Pinchot.Loc
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Cursor where
  prettyVal (Cursor filename loc) = Pretty.Rec "Penny.Cursor.Cursor"
    [ ("_filename", prettyText filename)
    , ("_loc", Pretty.prettyVal loc)
    ]

Lens.makeLenses ''Cursor
