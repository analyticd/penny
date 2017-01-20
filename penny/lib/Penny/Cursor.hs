{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Locations of items.

module Penny.Cursor where

import qualified Control.Lens as Lens
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty
import qualified Pinchot

import Penny.Pretty
import Penny.Prelude

data Cursor = Cursor
  { _collection :: Either Text FilePath
  -- ^ Indicates where this item came from; either it is an arbitrary
  -- 'Text' or a 'FilePath'.
  , _loc :: Pinchot.Loc
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Cursor where
  prettyVal (Cursor col loc) = Pretty.Rec "Cursor"
    [ ("_collection", prettyEither prettyText prettyFilePath col)
    , ("_loc", Pretty.prettyVal loc)
    ]

Lens.makeLenses ''Cursor
