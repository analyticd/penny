{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Transaction where

import Penny.Ents
import Penny.Tree
import qualified Penny.Fields as F

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (ZonedTime)

data TopLineOrPostline b a = TopLineOrPostline
  { _location :: a
  , _ancillary :: Seq Tree
  , _fields :: b
  } deriving (Functor, Foldable, Traversable)

emptyTopLineOrPostline :: b -> TopLineOrPostline b ()
emptyTopLineOrPostline = TopLineOrPostline () Seq.empty

Lens.makeLenses ''TopLineOrPostline

type Postline a = TopLineOrPostline F.PostingFields a
type TopLine a = TopLineOrPostline F.TopLineFields a

data Transaction a = Transaction
  { _topLine :: TopLine a
  , _postings :: Balanced (Postline a)
  }

Lens.makeLenses ''Transaction

emptyTopLine :: ZonedTime -> TopLine ()
emptyTopLine zt = emptyTopLineOrPostline (F.emptyTopLineFields zt)

emptyPostline :: Postline ()
emptyPostline = emptyTopLineOrPostline F.emptyPostingFields

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
