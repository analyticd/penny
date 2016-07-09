-- | Providing accessors (lenses and functions) to operate on the main
-- 'Posting' in a clatch-like type.  Every clatch-like type contains
-- one or more postings: the so-called @main@ posting, and the sibling
-- postings.  This module only handles the main posting, which is the
-- one that is typically the most interesting.
-- "Penny.Clatch.Access.Siblings" accesses the sibling postings.
module Penny.Clatch.Access.Posting where

import Control.Lens (Lens', view)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import Data.Text (Text)

import Penny.Account
import Penny.Clatch.Access.Slice
import Penny.Clatch.Types
import Penny.Core (Core, postline, Posting, postingMagnitude)
import qualified Penny.Core as Core
import Penny.Decimal
import Penny.Polar
import Penny.SeqUtil
import Penny.Serial
import qualified Penny.Tranche as Tranche
import Penny.Troika (Troika)

-- | Operates on the 'Posting' in the 'Slice'.
--
-- @
-- 'posting' :: 'Lens'' ('Sliced' a)    'Posting'
-- 'posting' :: 'Lens'' ('Converted' a) 'Posting'
-- 'posting' :: 'Lens'' ('Prefilt' a)   'Posting'
-- 'posting' :: 'Lens'' ('Sorted' a)    'Posting'
-- 'posting' :: 'Lens'' ('Totaled' a)   'Posting'
-- 'posting' :: 'Lens'' 'Clatch'        'Posting'
-- @

posting :: Lens' (a, (Slice (Posting l), b)) (Posting l)
posting = slice . onSlice

core :: Lens' (Sliced l a) Core
core = posting . Core.core

troika :: Lens' (Sliced l a) Troika
troika = core . Core.troika

birth :: Lens' (Sliced l a) Serset
birth = core . Core.birth

number :: Lens' (Sliced l a) (Maybe Integer)
number = posting . postline . Tranche.number

flag :: Lens' (Sliced l a) (Maybe Text)
flag = posting . postline . Tranche.flag

account :: Lens' (Sliced l a) Account
account =  posting . postline . Tranche.account

fitid :: Lens' (Sliced l a) (Maybe Text)
fitid = posting . postline . Tranche.fitid

tags :: Lens' (Sliced l a) (Seq Text)
tags = posting . postline . Tranche.tags

uid :: Lens' (Sliced l a) (Maybe Text)
uid = posting . postline . Tranche.uid

reconciled :: Sliced l a -> Bool
reconciled = Tranche.reconciled . view (posting . postline)

cleared :: Sliced l a -> Bool
cleared = Tranche.cleared . view (posting . postline)

side :: Sliced l a -> Maybe Pole
side = Core.postingSide . view posting

qty :: Sliced l a -> Decimal
qty = Core.postingQty . view posting

magnitude :: Sliced l a -> DecUnsigned
magnitude = postingMagnitude . view posting

isDebit :: Sliced l a -> Bool
isDebit = maybe False (== debit) . side

isCredit :: Sliced l a -> Bool
isCredit = maybe False (== credit) . side

isZero :: Sliced l a -> Bool
isZero = isNothing . side
