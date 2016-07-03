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

data Cursor = Cursor
  { _filename :: Text
  , _loc :: Pinchot.Loc
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Cursor where
  prettyVal x = Pretty.Rec "Penny.Cursor.Cursor"
    [ ("_filename", Pretty.prettyVal . _filename $ x)
    , ("_loc", maybe (error "Penny.Cursor.Cursor.prettyVal")
        id . Pretty.reify . _loc $ x)
    ]

Lens.makeLenses ''Cursor
