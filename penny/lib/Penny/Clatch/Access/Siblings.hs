-- | Accessing the sibling postings in a clatch-like type, using
-- lenses and functions.  Most of these values are not lenses, because
-- the siblings are contained in two separate values (one for siblings
-- on the left, one for siblings on the right).
module Penny.Clatch.Access.Siblings where

import Control.Lens (view)
import Data.Sequence (Seq)
import Data.Text (Text)

import Penny.Account
import Penny.Clatch.Types
import Penny.Core
import Penny.SeqUtil
import qualified Penny.Tranche as Tranche

-- | Gets all sibling 'Posting' in the 'Slice'.
--
-- @
-- 'siblings' :: 'Sliced' a -> 'Seq' 'Posting'
-- 'siblings' :: 'Converted' a -> 'Seq' 'Posting'
-- 'siblings' :: 'Prefilt' a -> 'Seq' 'Posting'
-- 'siblings' :: 'Sorted' a -> 'Seq' 'Posting'
-- 'siblings' :: 'Totaled' a -> 'Seq' 'Posting'
-- 'siblings' :: 'Clatch' a -> 'Seq' 'Posting'
-- @
siblings :: (a, (Slice (Posting l), b)) -> Seq (Posting l)
siblings s = l `mappend` r
  where
    Slice l _ r = fst . snd $ s

number :: Sliced l a -> Seq (Maybe Integer)
number = fmap (view (postline . Tranche.number)) . siblings

flag :: Sliced l a -> Seq (Maybe Text)
flag = fmap (view (postline . Tranche.flag)) . siblings

account :: Sliced l a -> Seq (Account)
account = fmap (view (postline. Tranche.account)) . siblings

fitid :: Sliced l a -> Seq (Maybe Text)
fitid = fmap (view (postline . Tranche.fitid)) . siblings

tags :: Sliced l a -> Seq (Seq Text)
tags = fmap (view (postline . Tranche.tags)) . siblings

reconciled :: Sliced l a -> Seq Bool
reconciled = fmap (Tranche.reconciled . view postline) . siblings

cleared :: Sliced l a -> Seq Bool
cleared = fmap (Tranche.cleared . view postline) . siblings
