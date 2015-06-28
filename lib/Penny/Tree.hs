{-# LANGUAGE TemplateHaskell #-}
module Penny.Tree where

import Control.Lens
import Penny.Realm
import Penny.Scalar
import Data.Sequence (Seq)

data Tree = Tree
  { _realm :: Realm
  , _scalar :: Maybe Scalar
  , _children :: Seq Tree
  } deriving (Eq, Ord, Show)

makeLenses ''Tree
