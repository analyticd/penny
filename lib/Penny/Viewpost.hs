{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Viewpost where

import Penny.Ledger
import Penny.SeqUtil
import Control.Lens
import Data.Foldable (Foldable)

data Viewpost l a = Viewpost
  { _viewpost :: View (PostingL l)
  , _viewpostee :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Viewpost
