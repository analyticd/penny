{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Tranche where

import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Control.Lens as Lens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal, Value)
import qualified Text.Show.Pretty as Pretty

import qualified Penny.Fields as F

-- | A 'Tranche' holds either top line fields or posting fields.
data Tranche b a = Tranche
  { _location :: a
  -- ^ Where something is located in a file.
  , _fields :: b
  -- ^ Field data.
  } deriving (Show, Functor, Foldable, Traversable, Generic)

prettyTranche
  :: (b -> Value)
  -> (a -> Value)
  -> Tranche b a
  -> Value
prettyTranche fb fa (Tranche a b) = Pretty.Rec "Tranche"
  [ ("_location", fa a)
  , ("_fields", fb b)
  ]

instance (PrettyVal b, PrettyVal a) => PrettyVal (Tranche b a) where
  prettyVal = prettyTranche Pretty.prettyVal Pretty.prettyVal

emptyTranche :: b -> Tranche b ()
emptyTranche = Tranche ()

Lens.makeLenses ''Tranche

type Postline a = Tranche F.PostingFields a

prettyPostline
  :: (a -> Value)
  -> Postline a
  -> Value
prettyPostline fa = prettyTranche Pretty.prettyVal fa

type TopLine a = Tranche F.TopLineFields a

prettyTopLine
  :: (a -> Value)
  -> TopLine a
  -> Value
prettyTopLine fa = prettyTranche Pretty.prettyVal fa

emptyTopLine :: Time.ZonedTime -> TopLine ()
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

payee :: forall a. Lens.Lens' (TopLine a) Text
payee = fields . F.payee

origPayee :: forall a. Lens.Lens' (TopLine a) Text
origPayee = fields . F.origPayee

origDate :: forall a. Lens.Lens' (Postline a) (Maybe Time.ZonedTime)
origDate = fields . F.origDate

number :: forall a. Lens.Lens' (Postline a) (Maybe Integer)
number = fields . F.number

flag :: forall a . Lens.Lens' (Postline a) Text
flag = fields . F.flag

account :: forall a. Lens.Lens' (Postline a) (Seq Text)
account = fields . F.account

fitid :: forall a. Lens.Lens' (Postline a) Text
fitid = fields . F.fitid

tags :: forall a. Lens.Lens' (Postline a) (Seq Text)
tags = fields . F.tags

uid :: forall a. Lens.Lens' (Postline a) Text
uid = fields . F.uid

reconciled :: Postline a -> Bool
reconciled = F.reconciled . _fields

cleared :: Postline a -> Bool
cleared = F.cleared . _fields
