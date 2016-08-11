{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Tree where

import Control.Lens hiding (children)
import Penny.Scalar
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

import Penny.Pretty

data Tree = Tree
  { _scalar :: Maybe Scalar
  , _children :: Seq Tree
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Tree where
  prettyVal (Tree sc cs) = Pretty.Rec "Tree"
    [ ("_scalar", prettyMaybe Pretty.prettyVal sc)
    , ("_children", prettySeq Pretty.prettyVal cs)
    ]

makeLenses ''Tree

