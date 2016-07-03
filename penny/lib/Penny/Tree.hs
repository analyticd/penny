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

data Tree = Tree
  { _scalar :: Maybe Scalar
  , _children :: Seq Tree
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Tree

makeLenses ''Tree

