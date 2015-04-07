module Penny.Offset where

-- | Things that have an offset.  Laws:
--
-- for all @x@, @offset x /= x@
--
-- for all @x@, @offset (offset x) == x@
--
-- For instance, 'Penny.Side.Side' can satisfy these laws, but
-- 'Int' cannot, because although @1@ can satisfy the law if @offset 1
-- == -1@, @offset 0@ cannot satisfy the first law.
class HasOffset a where
  offset :: a -> a

