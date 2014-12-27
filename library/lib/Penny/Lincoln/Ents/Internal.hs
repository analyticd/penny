module Penny.Lincoln.Ents.Internal where

import Control.Applicative
import Data.Sequence (Seq, viewl, ViewL(..), (<|), (|>))
import qualified Data.Sequence as S
import Penny.Lincoln.Ent
import Penny.Lincoln.Balances
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data Ents m = Ents
  { entsToSeqEnt :: Seq (Ent m)
  , entsToBalances :: Balances
  } deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f (Ents s b) = Ents (fmap (fmap f) s) b

instance Monoid (Ents m) where
  mempty = Ents mempty mempty
  mappend (Ents s1 b1) (Ents s2 b2) = Ents (s1 <> s2) (b1 <> b2)

instance F.Foldable Ents where
  foldr f z = F.foldr f z . fmap (\(Ent _ _ m) -> m) . entsToSeqEnt

instance T.Traversable Ents where
  sequenceA (Ents sq bl) = fmap (flip Ents bl) . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs

appendEnt :: Ents a -> Ent a -> Ents a
appendEnt (Ents s b) e@(Ent q c _) = Ents (s |> e)
  (b <> balancesFromPair q c)

prependEnt :: Ent a -> Ents a -> Ents a
prependEnt e@(Ent q c _) (Ents s b) = Ents (e <| s)
  (b <> balancesFromPair q c)

