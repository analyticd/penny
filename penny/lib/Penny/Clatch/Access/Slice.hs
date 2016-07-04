-- | Providing accessors (lenses and functions) to the 'Slice'
-- component of a clatch-like type.
module Penny.Clatch.Access.Slice where

import Control.Lens (Lens')
import qualified Control.Lens as Lens

import Penny.Clatch.Types
import Penny.Core
import Penny.SeqUtil

-- | Operate on the 'Slice'.
--
-- @
-- 'slice' :: 'Lens'' ('Sliced' a)    ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Converted' a) ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Prefilt' a)   ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Sorted' a)    ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Totaled' a)   ('Slice' 'Posting')
-- 'slice' :: 'Lens'' 'Clatch'        ('Slice' 'Posting')
-- @

slice :: Lens' (a, (Slice (Posting l), b)) (Slice (Posting l))
slice = Lens._2 . Lens._1
