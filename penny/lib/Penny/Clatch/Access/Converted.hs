-- | Accessors (lenses and functions) for the converted 'Amount' in a
-- clatch-like type.
module Penny.Clatch.Access.Converted where

import Control.Lens (Lens', view, (^.), to)
import qualified Control.Lens as Lens

import Penny.Amount
import Penny.Clatch.Access.Slice
import Penny.Core
import Penny.SeqUtil
import Penny.Troika (c'Amount'Troika)

-- | Operate on the converted 'Amount'.  There is no converted
-- 'Amount' if the 'Converter' did not specify a conversion.
--
-- @
-- 'converted' :: 'Lens'' ('Converted' a) ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' ('Prefilt' a)   ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' ('Sorted' a)    ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' ('Totaled' a)   ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' 'Clatch'        ('Maybe' 'Amount')
-- @

converted :: Lens' (a, (b, (Maybe Amount, c))) (Maybe Amount)
converted = Lens._2 . Lens._2 . Lens._1

-- | If there is a converted amount, then use that.  Otherwise, use
-- the original, unconverted amount.  This is not a 'Lens' but it is a
-- 'Getter'.
--
-- @
-- 'best' :: 'Getter' ('Converted' a) 'Amount'
-- 'best' :: 'Getter' ('Prefilt' a)   'Amount'
-- 'best' :: 'Getter' ('Sorted' a)    'Amount'
-- 'best' :: 'Getter' ('Totaled' a)   'Amount'
-- 'best' :: 'Getter' 'Clatch'        'Amount'
-- @

best :: (a, (Slice (Posting l), (Maybe Amount, c))) -> Amount
best clatch = case view converted clatch of
  Just a -> a
  Nothing -> clatch ^. slice . onSlice . core
    . troika . to c'Amount'Troika

