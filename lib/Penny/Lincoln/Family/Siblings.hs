{-# LANGUAGE DeriveGeneric #-}

module Penny.Lincoln.Family.Siblings (
  Siblings(Siblings, first, second, rest),
  collapse
  ) where

import qualified Prelude as P
import Prelude hiding (concat)
import qualified Data.Semigroup as S
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as T
import GHC.Generics (Generic)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Binary as B

-- | Describes the siblings of a family, but tells you nothing about
-- the parent. There are always at least two Siblings.
data Siblings a = Siblings { first :: a
                           , second :: a
                           , rest :: [a] }
                  deriving (Eq, Show, Generic)

instance B.Binary a => B.Binary (Siblings a)

instance S.Semigroup (Siblings a) where
  (Siblings a1 a2 ar) <> (Siblings b1 b2 br) =
    Siblings a1 a2 (ar ++ (b1:b2:br))

instance Functor Siblings where
  fmap g (Siblings f s rs) = Siblings (g f) (g s) (map g rs)

instance Foldable.Foldable Siblings where
  foldr g b (Siblings f s rs) = g f (g s (foldr g b rs))

instance T.Traversable Siblings where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse g (Siblings f s rs) =
    Siblings
    <$> g f
    <*> g s
    <*> T.traverse g rs

-- | Change a Siblings of NonEmpty lists to a Siblings. The original
-- order of the elements contained in the Siblings and within the
-- NonEmpty lists is preserved.
collapse :: Siblings (NE.NonEmpty a)
            -> Siblings a
collapse (Siblings (s1_1:|s1_r) s2@(s2_1:|s2_r) sr) =
  Siblings r1 r2 rr where
    r1 = s1_1
    (r2, rr) = case s1_r of
      [] -> (s2_1, (s2_r ++ concatNE sr))
      x:xs -> (x, xs ++ concatNE (s2 : sr))

concatNE :: [NE.NonEmpty a] -> [a]
concatNE = foldr f [] where
  f (a :| as) soFar = (a:as) ++ soFar
