{-# OPTIONS_HADDOCK not-home #-}
module Penny.Ents.Internal where

import Data.Sequence (Seq, viewl, ViewL(..), (<|), (|>)
                      , ViewR(..), viewr)
import qualified Data.Sequence as S
import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Commodity
import Penny.Qty
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Text as X
import Penny.Trio
import qualified Data.Map as M
import Penny.Side
import Penny.Representation
import Penny.Friendly

data Ents a = Ents
  { entsToSeqEnt :: Seq (Amount, a)
  , entsToImbalance :: Imbalance
  } deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f (Ents s b) = Ents (fmap (fmap f) s) b

instance Monoid (Ents m) where
  mempty = Ents mempty mempty
  mappend (Ents s1 b1) (Ents s2 b2) = Ents (s1 <> s2) (b1 <> b2)

instance F.Foldable Ents where
  foldr f z = F.foldr f z . fmap snd . entsToSeqEnt

instance T.Traversable Ents where
  sequenceA (Ents sq bl) = fmap (flip Ents bl) . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs

newtype Balanced a = Balanced { balancedToSeqEnt :: Seq (Amount, a) }
  deriving (Eq, Ord, Show)

instance Functor Balanced where
  fmap f (Balanced sq) = Balanced $ fmap (fmap f) sq

instance Monoid (Balanced a) where
  mempty = Balanced mempty
  mappend (Balanced x) (Balanced y) = Balanced (x <> y)

instance F.Foldable Balanced where
  foldr f z = F.foldr f z . fmap snd . balancedToSeqEnt

instance T.Traversable Balanced where
  sequenceA (Balanced sq) = fmap Balanced . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs


appendEnt :: Ents a -> (Amount, a) -> Ents a
appendEnt (Ents s b) e = Ents (s |> e)
  (b <> c'Imbalance'Amount (fst e))

prependEnt :: (Amount, a) -> Ents a -> Ents a
prependEnt e (Ents s b) = Ents (e <| s)
  (b <> c'Imbalance'Amount (fst e))

appendTrio :: Ents a -> Trio -> Either TrioError (a -> Ents a)
appendTrio ents@(Ents _ imb) trio =
  case trioToAmount imb trio of
    Left e -> Left e
    Right g -> Right $ \a -> appendEnt ents (g, a)

prependTrio :: Trio -> Ents a -> Either TrioError (a -> Ents a)
prependTrio trio ents@(Ents _ imb) =
  case trioToAmount imb trio of
    Left e -> Left e
    Right g -> Right $ \a -> prependEnt (g, a) ents

data ImbalancedError
  = ImbalancedError (Commodity, QtyNonZero) [(Commodity, QtyNonZero)]
  deriving (Eq, Ord, Show)

instance Friendly ImbalancedError where
  friendly (ImbalancedError c1 cs) =
    [ "Transaction is not balanced.  Imbalance:"
    , showImb c1
    ] ++ map showImb cs
    where
      showImb (Commodity cy, q)
        = "  " ++ X.unpack cy ++ " " ++ displayQtyNonZero q

entsToBalanced :: Ents a -> Either ImbalancedError (Balanced a)
entsToBalanced (Ents sq (Imbalance m)) = case M.toList m of
  [] -> return $ Balanced sq
  x:xs -> Left $ ImbalancedError x xs

data EntView a = EntView
  { onLeft :: Seq (Amount, a)
  , onView :: (Amount, a)
  , onRight :: Seq (Amount, a)
  } deriving (Eq, Ord, Show)

instance Functor EntView where
  fmap f (EntView l c r) = EntView (fmap (fmap f) l) (fmap f c)
    (fmap (fmap f) r)

instance F.Foldable EntView where
  foldr f z (EntView l c r) = F.foldr f z
    . fmap snd $ (l |> c) <> r

instance T.Traversable EntView where
  traverse f (EntView l c r)
    = EntView
    <$> T.sequenceA (fmap (T.traverse f) l)
    <*> T.traverse f c
    <*> T.sequenceA (fmap (T.traverse f) r)

moveLeft :: EntView a -> Maybe (EntView a)
moveLeft (EntView l c r) = case viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ EntView xs x (c <| r)

moveRight :: EntView a -> Maybe (EntView a)
moveRight (EntView l c r) = case viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ EntView (l |> c) x xs

-- siblingEntViews - careful - each sibling view must contain the 'Ent'
-- for the current 'EntView', but the returned 'Seq' cannot itself
-- include the current 'EntView'

-- | A 'Seq' of 'EntView' that are siblings of this 'EntView'.
siblingEntViews :: EntView a -> Seq (EntView a)
siblingEntViews (EntView l c r) = go S.empty pairs
  where
    pairs = ( (fmap (\e -> (True, e)) l)
              |> (False, c) )
            <> fmap (\e -> (True, e)) r
    go soFar sq = case viewl sq of
      EmptyL -> soFar
      (use, e) :< rest
        | not use -> go soFar sq
        | otherwise -> this <| go (soFar |> this) rest
        where
          this = EntView l' e r'
          l' = fmap onView soFar
          r' = fmap snd rest


allEntViews :: Balanced a -> Seq (EntView a)
allEntViews (Balanced sq) = go S.empty sq
  where
    go onLeft s = case viewl s of
      EmptyL -> S.empty
      x :< xs -> EntView onLeft x xs <| go (onLeft |> x) xs

viewToBalanced :: EntView a -> Balanced a
viewToBalanced (EntView l c r) = Balanced $ (l |> c) <> r

-- | Creates 'Balanced' sets of 'Ent'.  Unlike 'entsToBalanced' this
-- function never fails.  To accomplish this, it places greater
-- restrictions on its arguments than does 'entsToBalanced'.
restrictedBalanced
  :: Commodity
  -- ^ All postings will have this commodity.
  -> Side
  -- ^ Each posting (except the last one) will have this side.
  -> Seq (NilOrBrimScalarAnyRadix, Arrangement, a)
  -- ^ Each posting, along with its metadata and 'Arrangement'.
  -> a
  -- ^ Metadata for the last posting
  -> Balanced (a, Trio)
  -- ^ Each posting given in the 'Seq' above will have a corresponding
  -- value in the 'Seq' in this 'Balanced'.  Each of these 'Ent' will
  -- have a 'Trio' whose constructor is 'QC'.  In addition, there will
  -- be a final 'Ent' whose 'Trio' is constructed with 'E'.  This
  -- final 'Ent' will always be appended even if it is not necessary;
  -- that is, its 'Qty' may have a significand of 0.
restrictedBalanced cy s sq metaLast = Balanced (begin |> end)
  where
    begin = fmap f sq
    f (rep, ar, meta) = (Amount cy (toQty q), (meta, tri))
      where
        q = nilOrBrimScalarAnyRadixToQtyRepAnyRadix s rep
        tri = QC q cy ar
    end = (Amount cy (negate tot), (metaLast, E))
    tot = F.foldl' (+) (fromInteger 0) . fmap (\(Amount _ q, _) -> q)
      $ begin
