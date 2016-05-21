{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Penny.Tree where

import Control.Lens hiding (children)
import Penny.Realm
import Penny.Scalar
import Data.Sequence (Seq)

data Tree = Tree
  { _realm :: Realm
  , _scalar :: Maybe Scalar
  , _children :: Seq Tree
  } deriving (Eq, Ord, Show)

makeLenses ''Tree

