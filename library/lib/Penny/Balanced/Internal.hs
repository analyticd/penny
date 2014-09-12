{-# OPTIONS_HADDOCK not-home #-}
module Penny.Balanced.Internal where

import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Ent as Ent
import Data.Monoid
import Data.Traversable
import Data.Foldable
import qualified Data.Foldable as F
import Control.Applicative

data T m = T { toSeq :: Seq (Ent.T m) }
  deriving (Eq, Ord, Show)

instance Functor T where
  fmap f (T s) = T (fmap (fmap f) s)

instance Monoid (T m) where
  mempty = T mempty
  mappend (T s1) (T s2) = T (s1 <> s2)

instance Foldable T where
  foldr f z = F.foldr f z . fmap Ent.meta . toSeq

instance Traversable T where
  sequenceA (T sq) = fmap T . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> sequenceA e <*> go xs
