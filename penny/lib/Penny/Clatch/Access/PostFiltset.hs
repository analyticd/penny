-- | Accessors (lenses and functions) to operate on the 'PostFiltset' of a
-- clatch-like type.

module Penny.Clatch.Access.PostFiltset where

import Control.Lens (Lens', _2, _1)
import Penny.Clatch.Types

-- | Operate on the 'PostFiltset'.
--
-- @
-- 'postFiltset' :: 'Lens'' 'Clatch' 'PostFiltset'
-- @
postFiltset :: Lens' (a, (b, (c, (d, (e, (f, (PostFiltset, g))))))) PostFiltset
postFiltset =  _2 . _2 . _2 . _2 . _2 . _2 . _1
