{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Penny.Tranche where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (ZonedTime)
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

emptyTopLine :: ZonedTime -> TopLine ()
emptyTopLine zt = emptyTranche (F.emptyTopLineFields zt)

emptyPostline :: Postline ()
emptyPostline = emptyTranche F.emptyPostingFields

date :: forall a. Lens.Lens' (TopLine a) ZonedTime
date = fields . F.date

payee :: forall a. Lens.Lens' (TopLine a) (Maybe Text)
payee = fields . F.payee

number :: forall a. Lens.Lens' (Postline a) (Maybe Integer)
number = fields . F.number

flag :: forall a . Lens.Lens' (Postline a) (Maybe Text)
flag = fields . F.flag

account :: forall a. Lens.Lens' (Postline a) (Seq Text)
account = fields . F.account

fitid :: forall a. Lens.Lens' (Postline a) (Maybe Text)
fitid = fields . F.fitid

tags :: forall a. Lens.Lens' (Postline a) (Seq Text)
tags = fields . F.tags
