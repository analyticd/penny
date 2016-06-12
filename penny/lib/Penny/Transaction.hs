{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Transaction where

import Penny.Ents
import qualified Penny.Fields as F
import Penny.Tranche

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (ZonedTime)

data Transaction a = Transaction
  { _topLine :: TopLine a
  , _postings :: Balanced (Postline a)
  }

Lens.makeLenses ''Transaction

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
