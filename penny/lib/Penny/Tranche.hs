{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module Penny.Tranche where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Control.Lens as Lens

import qualified Penny.Fields as F
import Penny.Tree

data Tranche b a = Tranche
  { _location :: a
  , _ancillary :: Seq Tree
  , _fields :: b
  } deriving (Functor, Foldable, Traversable)

emptyTranche :: b -> Tranche b ()
emptyTranche = Tranche () Seq.empty

Lens.makeLenses ''Tranche

type Postline a = Tranche F.PostingFields a
type TopLine a = Tranche F.TopLineFields a

