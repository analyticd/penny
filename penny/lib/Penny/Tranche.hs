{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Tranche where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Control.Lens as Lens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

import qualified Penny.Fields as F
import Penny.Tree

-- | A 'Tranche' holds either top line fields or posting fields.
data Tranche b a = Tranche
  { _location :: a
  -- ^ Where something is located in a file.
  , _ancillary :: Seq Tree
  -- ^ Additional fields that were in the ledger file, but that are
  -- not one of the usual fields.
  , _fields :: b
  -- ^ Field data.
  } deriving (Functor, Foldable, Traversable, Generic)

instance (PrettyVal b, PrettyVal a) => PrettyVal (Tranche b a)

emptyTranche :: b -> Tranche b ()
emptyTranche = Tranche () Seq.empty

Lens.makeLenses ''Tranche

type Postline a = Tranche F.PostingFields a
type TopLine a = Tranche F.TopLineFields a

emptyTopLine :: ZonedTime -> TopLine ()
emptyTopLine zt = emptyTranche (F.emptyTopLineFields zt)

emptyPostline :: Postline ()
emptyPostline = emptyTranche mempty

zonedTime :: forall a. Lens.Lens' (TopLine a) ZonedTime
zonedTime = fields . F.zonedTime

day :: forall a. Lens.Lens' (TopLine a) (Time.Day)
day = fields . F.day

timeOfDay :: forall a. Lens.Lens' (TopLine a) (Time.TimeOfDay)
timeOfDay = fields . F.timeOfDay

timeZone :: forall a. Lens.Lens' (TopLine a) (Time.TimeZone)
timeZone = fields . F.timeZone

timeZoneMinutes :: forall a. Lens.Lens' (TopLine a) Int
timeZoneMinutes = fields . F.timeZoneMinutes

payee :: forall a. Lens.Lens' (TopLine a) (Maybe Text)
payee = fields . F.payee

origPayee :: forall a. Lens.Lens' (TopLine a) (Maybe Text)
origPayee = fields . F.origPayee

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

uid :: forall a. Lens.Lens' (Postline a) (Maybe Text)
uid = fields . F.uid

reconciled :: Postline a -> Bool
reconciled = F.reconciled . _fields

cleared :: Postline a -> Bool
cleared = F.cleared . _fields
