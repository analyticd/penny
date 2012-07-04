module Penny.Lincoln.Family.Family where

import qualified Data.Functor.Identity as I

-- | A Family has one parent (ah, the anomie, sorry) and at least two
-- children.
data Family p c =
  Family { parent :: p
         , child1 :: c
         , child2 :: c
         , children :: [c] }
  deriving (Eq, Show)

-- | Maps over all children in a monad, in order starting with child
-- 1, then child 2, then the children in the list from left to right.
mapChildrenM ::
  Monad m
  => (a -> m b)
  -> Family p a
  -> m (Family p b)
mapChildrenM f (Family p c1 c2 cs) = do
  c1' <- f c1
  c2' <- f c2
  cs' <- mapM f cs
  return $ Family p c1' c2' cs'


-- | Maps over all children, in order starting with child
-- 1, then child 2, then the children in the list from left to right.
mapChildren ::
  (a -> b)
  -> Family p a
  -> Family p b
mapChildren f fam = I.runIdentity (mapChildrenM f' fam) where
  f' = return . f

-- | Maps over the parent in a monad.
mapParentM ::
  Monad m
  => (a -> m b)
  -> Family a c
  -> m (Family b c)
mapParentM f (Family p c1 c2 cs) = do
  p' <- f p
  return $ Family  p' c1 c2 cs

-- | Maps over the parent.
mapParent :: (a -> b) -> Family a c -> Family b c
mapParent f fam = I.runIdentity (mapParentM f' fam) where
  f' = return . f
