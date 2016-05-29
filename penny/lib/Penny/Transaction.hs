{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Transaction where

import Penny.Ents
import Penny.Fields
import Penny.Tree
import qualified Penny.Fields as F

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (ZonedTime)

data TopLineOrPosting b a = TopLineOrPosting
  { _location :: a
  , _ancillary :: Seq Tree
  , _fields :: b
  } deriving (Functor, Foldable, Traversable)

emptyTopLineOrPosting :: b -> TopLineOrPosting b ()
emptyTopLineOrPosting = TopLineOrPosting () Seq.empty

Lens.makeLenses ''TopLineOrPosting

type Posting a = TopLineOrPosting F.PostingFields a
type TopLine a = TopLineOrPosting F.TopLineFields a

data Transaction a = Transaction
  { _topLine :: TopLine a
  , _postings :: Balanced (Posting a)
  }

Lens.makeLenses ''Transaction

emptyTopLine :: ZonedTime -> TopLine ()
emptyTopLine zt = emptyTopLineOrPosting (F.emptyTopLineFields zt)

emptyPosting :: Posting ()
emptyPosting = emptyTopLineOrPosting F.emptyPostingFields

date :: forall a. Lens.Lens' (TopLine a) ZonedTime
date = fields . F.date

payee :: forall a. Lens.Lens' (TopLine a) (Maybe Text)
payee = fields . F.payee

number :: forall a. Lens.Lens' (Posting a) (Maybe Integer)
number = fields . F.number

flag :: forall a . Lens.Lens' (Posting a) (Maybe Text)
flag = fields . F.flag

account :: forall a. Lens.Lens' (Posting a) (Seq Text)
account = fields . F.account

fitid :: forall a. Lens.Lens' (Posting a) (Maybe Text)
fitid = fields . F.fitid

tags :: forall a. Lens.Lens' (Posting a) (Seq Text)
tags = fields . F.tags
