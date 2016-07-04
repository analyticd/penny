module Penny.Clatch.Types where

import Penny.Amount
import Penny.Balance
import Penny.Core
import Penny.SeqUtil
import Penny.Serial

-- | The 'Serset' after all postings have been pre-filtered.
type PreFiltset = Serset

-- | The 'Serset' after all slices have been sorted.
type Sortset = Serset

-- | The 'Serset' after the sorted slices have been post-filtered.
type PostFiltset = Serset

-- # Clatches and compatible types

-- | A single 'Slice' 'Posting' contains not only the 'Posting' but
-- also all sibling 'Posting's.  A 'Transaction' can give rise to
-- multiple 'Slice's, and therefore to mulitple 'Sliced'.
type Sliced l a = (Transaction l, (Slice (Posting l), a))

-- | After 'Sliced' are created, the posting's 'Amount' is converted
-- using the specified 'Converter'.  There might not be any conversion
-- if the 'Converter' does not perform one.
type Converted l a = (Transaction l, (Slice (Posting l), (Maybe Amount, a)))

-- | After 'Converted' are created, they are filtered.  After
-- filtering, a 'PreFiltset' is assigned.
type Prefilt l a = (Transaction l, (Slice (Posting l),
  (Maybe Amount, (PreFiltset, a))))

-- | After the 'Prefilt' are created, they are sorted.  After sorting
-- a 'Sortset' is assigned.
type Sorted l a = (Transaction l, (Slice (Posting l), (Maybe Amount,
  (PreFiltset, (Sortset, a)))))

-- | After the 'Sorted' are created, the running balance is calculated
-- for each 'Sorted'.
type Totaled l a = (Transaction l, (Slice (Posting l),
  (Maybe Amount, (PreFiltset, (Sortset, (Balance, a))))))

-- | After 'Totaled' are created, they are filtered.  After filtering
-- a 'PostFiltset' is assigned.
type Clatch l =
  (Transaction l, (Slice (Posting l), (Maybe Amount, (PreFiltset, (Sortset,
    (Balance, (PostFiltset, ())))))))
