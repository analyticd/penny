-- | Providing accessors (lenses and functions) to operate on the main
-- 'Posting' in a clatch-like type.  Every clatch-like type contains
-- one or more postings: the so-called @main@ posting, and the sibling
-- postings.  This module only handles the main posting, which is the
-- one that is typically the most interesting.
-- "Penny.Clatch.Access.Siblings" accesses the sibling postings.
module Penny.Clatch.Access.Posting where

import Control.Lens (Lens', view)
import Data.Sequence (Seq)
import Data.Text (Text)

import Penny.Account
import Penny.Clatch.Access.Slice
import Penny.Clatch.Types
import Penny.Core
import Penny.SeqUtil
import qualified Penny.Tranche as Tranche

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

reconciled :: Sliced l a -> Bool
reconciled = Tranche.reconciled . view (posting . postline)

cleared :: Sliced l a -> Bool
cleared = Tranche.cleared . view (posting . postline)
