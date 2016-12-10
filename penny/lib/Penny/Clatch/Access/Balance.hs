-- | Accessors (lenses and functions) to operate on the 'Balance' of a
-- clatch-like type.

module Penny.Clatch.Access.Balance where

import Control.Lens (Lens', _2, _1)
import Penny.Balance

-- | Operate on the 'Balance'.  This is the running balance, not the
-- balance of a single posting.
--
-- @
-- 'balance' :: 'Lens'' ('Penny.Clatch.Types.Totaled' a) 'Balance'
-- 'balance' :: 'Lens'' 'Penny.Clatch.Types.Clatch'      'Balance'
-- @

balance :: Lens' (a, (b, (c, (d, (e, (Balance, f)))))) Balance
balance = _2 . _2 . _2 . _2 . _2 . _1
