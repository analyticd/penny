-- | Accessors (lenses and functions) to operate on the 'Sortset' of a
-- clatch-like type.
module Penny.Clatch.Access.Sortset where

import Control.Lens (Lens', _1, _2)
import Penny.Clatch.Types

-- | Operate on the 'Sortset'.
--
-- @
-- 'sortset' :: 'Lens'' ('Sorted' a)  'Sortset'
-- 'sortset' :: 'Lens'' ('Totaled' a) 'Sortset'
-- 'sortset' :: 'Lens'' 'Clatch'      'Sortset'
-- @

sortset :: Lens' (a, (b, (c, (d, (Sortset, e))))) Sortset
sortset = _2 . _2 . _2 . _2 . _1
