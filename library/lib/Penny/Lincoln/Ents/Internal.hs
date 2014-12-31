module Penny.Lincoln.Ents.Internal where

import Control.Applicative
import Data.Sequence (Seq, viewl, ViewL(..), (<|), (|>)
                      , ViewR(..), viewr)
import qualified Data.Sequence as S
import Penny.Lincoln.Ent
import Penny.Lincoln.Balances
import Penny.Lincoln.Commodity
import Penny.Lincoln.Qty
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Penny.Lincoln.Trio
import qualified Data.Map as M

data Ents m = Ents
  { entsToSeqEnt :: Seq (Ent m)
  , entsToImbalances :: Imbalances
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

newtype Balanced m = Balanced { balancedToSeqEnt :: Seq (Ent m) }
  deriving (Eq, Ord, Show)

instance Functor Balanced where
  fmap f (Balanced sq) = Balanced $ fmap (fmap f) sq

instance Monoid (Balanced a) where
  mempty = Balanced mempty
  mappend (Balanced x) (Balanced y) = Balanced (x <> y)

instance F.Foldable Balanced where
  foldr f z = F.foldr f z . fmap (\(Ent _ _ m) -> m) . balancedToSeqEnt

instance T.Traversable Balanced where
  sequenceA (Balanced sq) = fmap Balanced . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs


appendEnt :: Ents a -> Ent a -> Ents a
appendEnt (Ents s b) e@(Ent q c _) = Ents (s |> e)
  (b <> imbalancesFromPair q c)

prependEnt :: Ent a -> Ents a -> Ents a
prependEnt e@(Ent q c _) (Ents s b) = Ents (e <| s)
  (b <> imbalancesFromPair q c)

appendTrio :: Ents a -> Trio -> Either TrioError (a -> Ents a)
appendTrio ents@(Ents _ imb) trio =
  case toEnt imb trio of
    Left e -> Left e
    Right g -> Right $ appendEnt ents . g

prependTrio :: Trio -> Ents a -> Either TrioError (a -> Ents a)
prependTrio trio ents@(Ents _ imb) =
  case toEnt imb trio of
    Left e -> Left e
    Right g -> Right $ \a -> prependEnt (g a) ents

data ImbalancedError
  = ImbalancedError (Commodity, QtyNonZero) [(Commodity, QtyNonZero)]
  deriving (Eq, Ord, Show)

entsToBalanced :: Ents a -> Either ImbalancedError (Balanced a)
entsToBalanced (Ents sq (Imbalances m)) = case M.toList m of
  [] -> return $ Balanced sq
  x:xs -> Left $ ImbalancedError x xs

data View a = View
  { entsLeft :: Seq (Ent a)
  , thisEnt :: Ent a
  , entsRight :: Seq (Ent a)
  } deriving (Eq, Ord, Show)

instance Functor View where
  fmap f (View l c r) = View (fmap (fmap f) l) (fmap f c)
    (fmap (fmap f) r)

instance F.Foldable View where
  foldr f z (View l c r) = F.foldr f z
    . fmap (\(Ent _ _ m) -> m) $ (l |> c) <> r

instance T.Traversable View where
  traverse f (View l c r)
    = View
    <$> T.sequenceA (fmap (T.traverse f) l)
    <*> T.traverse f c
    <*> T.sequenceA (fmap (T.traverse f) r)

moveLeft :: View a -> Maybe (View a)
moveLeft (View l c r) = case viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x (c <| r)

moveRight :: View a -> Maybe (View a)
moveRight (View l c r) = case viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ View (l |> c) x xs

allViews :: Balanced a -> Seq (View a)
allViews (Balanced sq) = go S.empty sq
  where
    go onLeft s = case viewl s of
      EmptyL -> S.empty
      x :< xs -> View onLeft x xs <| go (onLeft |> x) xs

viewToBalanced :: View a -> Balanced a
viewToBalanced (View l c r) = Balanced $ (l |> c) <> r
