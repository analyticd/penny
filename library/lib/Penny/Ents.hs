module Penny.Ents
  ( T
  , ents
  , bals
  ) where

import Data.Sequence (Seq, viewl, ViewL(..), (<|))
import qualified Penny.Balances as Bals
import qualified Penny.Ent as Ent
import Data.Monoid
import Data.Foldable
import qualified Data.Foldable as F
import Data.Traversable
import Control.Applicative

data T m = T
  { ents :: Seq (Ent.T m)
  , bals :: Bals.T
  } deriving (Eq, Ord, Show)

instance Functor T where
  fmap f (T s b) = T (fmap (fmap f) s) b

instance Monoid (T m) where
  mempty = T mempty mempty
  mappend (T s1 b1) (T s2 b2) = T (s1 <> s2) (b1 <> b2)

instance Foldable T where
  foldr f z = F.foldr f z . fmap Ent.meta . ents

instance Traversable T where
  sequenceA (T sq bl) = fmap (flip T bl) . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure empty
        e :< xs ->
          (<|) <$> sequenceA e <*> go xs
