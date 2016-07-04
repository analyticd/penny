-- | Accessors (lenses and functions) to operate on the 'PreFiltset'
-- part of a clatch-like type.
module Penny.Clatch.Access.PreFiltset where

import Control.Lens (Lens', _2, _1)

import Penny.Clatch.Types

-- | Operate on the 'PreFiltset'.
--
-- @
-- 'preFiltset' :: 'Lens'' ('Prefilt' a) 'PreFiltset'
-- 'preFiltset' :: 'Lens'' ('Sorted' a)  'PreFiltset'
-- 'preFiltset' :: 'Lens'' ('Totaled' a) 'PreFiltset'
-- 'preFiltset' :: 'Lens'' 'Clatch'      'PreFiltset'
-- @

preFiltset :: Lens' (a, (b, (c, (PreFiltset, d)))) PreFiltset
preFiltset = _2 . _2 . _2 . _1
