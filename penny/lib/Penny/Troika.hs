{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | "Penny.Trio" hews closely to the possible manifestations of the
-- quantity, commodity, and arrangement in the ledger file.  The
-- "Penny.Trio" corresponds only to what is in a single posting.
--
-- The 'Troika' is concerned with holding the manifestation of the
-- quantity, commodity and arrangement, but after considering all
-- the postings of a single transaction together.  The biggest
-- consequence of this is that, unlike a "Penny.Trio", a 'Troika'
-- always contains a 'Commodity'.  In addition, the quantity in a
-- 'Troika' always is positive, negative, or zero, while the
-- quantity in a "Penny.Trio" is sometimes unsigned.
--
-- Also, the 'Troika' always has a quantity, while sometimes a
-- "Penny.Trio" has no quantity if the user did not enter one.
-- Therefore, the quantity in the 'Troika' sometimes is one of the
-- types from "Penny.Decimal" is the quantity was calculated rather
-- tha entered by the user.  It will ultimately be one of the types from
-- "Penny.Copper.Types" if the user did enter it.
module Penny.Troika where

import qualified Penny.Amount as A
import Penny.Arrangement
import Penny.Commodity
import Penny.Decimal
import Penny.Copper.Types (GrpRadCom, GrpRadPer)
import Penny.NonZero
import Penny.Polar
import Penny.Rep

import Control.Lens
import Data.Sequence (Seq)

data Troiload
  = QC RepAnyRadix Arrangement
  | Q RepAnyRadix
  | SC DecNonZero
  | S DecNonZero
  | UC BrimAnyRadix Pole Arrangement
  | NC NilAnyRadix Arrangement
  | US BrimAnyRadix Pole
  | UU NilAnyRadix
  | C DecNonZero
  | E DecNonZero
  deriving Show

troiloadPole :: Troiload -> Maybe Pole
troiloadPole x = case x of
  QC q _ -> pole'RepAnyRadix q
  Q q -> pole'RepAnyRadix q
  SC qnz -> Just . nonZeroSign . _coefficient $ qnz
  S qnz -> Just . nonZeroSign . _coefficient $ qnz
  UC _ s _ -> Just s
  NC _ _ -> Nothing
  US _ s -> Just s
  UU _ -> Nothing
  C qnz -> Just . nonZeroSign . _coefficient $ qnz
  E qnz -> Just . nonZeroSign . _coefficient $ qnz

type Troiquant = Either Troiload Decimal

data Troika = Troika
  { _commodity :: Commodity
  , _troiquant :: Either Troiload Decimal
  } deriving Show

makeLenses ''Troika

c'Troika'Amount :: A.Amount -> Troika
c'Troika'Amount (A.Amount cy q) = Troika cy (Right q)

-- TODO the () type is too specific
troikaRendering
  :: Troika
  -> Maybe (Commodity, Arrangement,
            Either (Seq (GrpRadCom Char ())) (Seq (GrpRadPer Char ())))
troikaRendering (Troika cy tq) = case tq of
  Left tl -> case tl of
    QC qr ar -> Just (cy, ar, ei)
      where
        ei = groupers'RepAnyRadix qr
    UC rnn _ ar -> Just (cy, ar, ei)
      where
        ei = groupers'BrimAnyRadix rnn
    _ -> Nothing
  _ -> Nothing
