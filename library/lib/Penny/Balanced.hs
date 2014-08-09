-- | Sets of entries.  Though every 'Ent' in an 'Ents' is valid, the
-- entire set is not necessarily balanced.  All ents in a 'Balanced'
-- are balanced.
module Penny.Balanced
  (
  -- * Ents
    Ents
  , entsSeq
  , entsBal
  , appendEnt
  , appendQCEnt

  -- * Balanced
  , Balanced
  , balancedEnts
  , balanced
  , mapV
  , rBalanced
  , mapEnts

  -- * Views
  , View
  , viewL
  , viewR
  , allViews
  , unView
  , viewLeft
  , viewCurrent
  , viewRight
  , siblings
  , moveLeft
  , moveRight
  , changeCurrent
  , changeCurrentMeta
  ) where


-- # Imports

import Penny.Balance
import Penny.Common
import Penny.Numbers.Qty
import Data.Monoid
import Prelude hiding (negate, sequence)
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import Data.Sequence (Seq, (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import qualified Data.Foldable as F
import qualified Data.Map as M
import Control.Applicative
import Penny.Ent

-- | Zero or more 'Ent'; always balanced.  That is, every
-- 'Debit' of a particular 'Commodity' is offset by an equal number of
-- 'Credit's of the same 'Commodity', and vice versa.

data Balanced m = Balanced { balancedEnts :: Seq (Ent m) }
  deriving (Eq, Ord, Show)

instance Functor Balanced where
  fmap f (Balanced sq) = Balanced (fmap (fmap f) sq)

instance Monoid (Balanced m) where
  mempty = Balanced mempty
  mappend (Balanced l) (Balanced r) = Balanced $ l <> r

instance F.Foldable Balanced where
  foldr f z = F.foldr f z . fmap entMeta . balancedEnts

instance Tr.Traversable Balanced where
  sequenceA = fmap Balanced . go . balancedEnts
    where
      go es = case S.viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> Tr.sequenceA e <*> go xs


-- | Zero or more 'Ent'.  Not necessarily balanced; maintains its own
-- running balance.
data Ents m = Ents { entsSeq :: Seq (Ent m)
                   , entsBal :: Balances }
  deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f (Ents sq bl) = Ents (fmap (fmap f) sq) bl

instance Monoid (Ents m) where
  mempty = Ents mempty mempty
  mappend (Ents s1 b1) (Ents s2 b2) = Ents (s1 <> s2) (b1 <> b2)

instance F.Foldable Ents where
  foldr f z = F.foldr f z . fmap entMeta . entsSeq

instance Tr.Traversable Ents where
  sequenceA (Ents sq bl) = fmap (flip Ents bl) . go $ sq
    where
      go es = case S.viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> Tr.sequenceA e <*> go xs


balanced :: Ents m -> Either Imbalances (Balanced m)
balanced (Ents sq bl)
  | M.null (unImbalances imb) = Left imb
  | otherwise = Right (Balanced sq)
  where
    imb = onlyUnbalanced bl

-- | Transform each 'Ent'.  Currently useful only with 'rearrange'.
mapEnts :: (Ent a -> Ent b) -> Balanced a -> Balanced b
mapEnts f (Balanced es) = Balanced $ go es
  where
    go sq = case S.viewl sq of
      EmptyL -> mempty
      e :< rest -> f e <| go rest

-- | Change the metadata on each 'Ent', using a function that has a
-- view of the entire 'Ent'.  Like 'fmap' but gives a view of the
-- whole 'Ent' rather than just the metadata.
mapV :: (Ent a -> b) -> Balanced a -> Balanced b
mapV f = Balanced . go . balancedEnts
  where
    go s = case S.viewl s of
      EmptyL -> S.empty
      w :< xs -> (fmap (const (f w)) w) <| go xs

appendEnt
  :: (Imbalances -> (Either a (Ent m)))
  -> Ents m
  -> Either a (Ents m)
appendEnt f (Ents sq bl) = fmap go $ f (onlyUnbalanced bl)
  where
    go e' = Ents (sq |> e')
      (bl <> balance (entCommodity e') (entQty e'))

appendQCEnt
  :: Either (Polar Period Side) (Polar Comma Side)
  -> Commodity
  -> m
  -> Ents m
  -> Ents m
appendQCEnt ei cy m (Ents sq bl) = Ents (sq |> e') bl'
  where
    e' = mkQCEnt ei cy m
    bl' = bl <> balance (entCommodity e') (entQty e')

-- | Creates 'Ents' but, unlike 'appendEnt', never fails.  To make this
-- guarantee, 'rBalanced' puts restrictions on its arguments.

rBalanced
  :: Side
  -- ^ All postings that you supply are on this 'Side'

  -> Commodity
  -- ^ All postings will have this same 'Commodity'

  -> a
  -- ^ Metadata for inferred posting

  -> Seq (Either (Unpolar Period) (Unpolar Comma), a)
  -- ^ Each initial member, and its corresponding metadata

  -> Balanced a
  -- ^ The resulting 'Ents'.  If the list of 'NZGrouped' was
  -- non-empty, then a single inferred posting is at the end to
  -- balance out the other non-zero postings.  Each 'Ent' has an
  -- 'Entrio' of 'SZC', except for the inferred posting, which is 'E'.

rBalanced s cy meta ls
  | S.null ls = Balanced mempty
  | otherwise = Balanced $ sq |> inferred
  where
    sq = fmap mkE ls
    mkE (ei, mt) = Ent q cy mt
      where
        q = either (unpolarToQty s) (unpolarToQty s) ei
    inferred = Ent q cy meta
      where
        q = Qty . negate . F.foldl' (+) zero
          . fmap (unQty . entQty) $ sq

-- | A single 'Ent' with information about how it relates to
-- surrounding 'Ent' in the 'Balanced'.
data View a = View
  { viewLeft :: Seq (Ent a)
  -- ^ These 'Ent' are to the left of the 'viewCurrent' 'Ent'.  Closer
  -- 'Ent' are at the right end of the 'Seq'.

  , viewCurrent :: Ent a
  -- ^ The 'Ent' you are currently viewing

  , viewRight :: Seq (Ent a)
  -- ^ These 'Ent' are to the right of the 'viewCurrent' 'Ent'.
  -- Closer 'Ent' are at the left end of the 'Seq'.

  } deriving (Eq, Ord, Show)

instance Functor View where
  fmap f (View l c r) = View (fmap (fmap f) l) (fmap f c)
    (fmap (fmap f) r)

instance F.Foldable View where
  foldr f z (View l c r) = F.foldr f z
    (fmap entMeta $ l <> S.singleton c <> r)

instance Tr.Traversable View where
  sequenceA v = View
    <$> trSeq (viewLeft v)
    <*> Tr.sequenceA (viewCurrent v)
    <*> trSeq (viewRight v)
    where
      trSeq sq = case S.viewl sq of
        EmptyL -> pure S.empty
        e :< es -> (<|) <$> Tr.sequenceA e <*> trSeq es

-- | All 'Ent' that neighbor the 'viewCurrent' 'Ent'.  The list is
-- ordered with all 'Ent' from left to right; the 'viewCurrent' 'Ent'
-- is not in the resulting list.
siblings :: View a -> Seq (Ent a)
siblings (View l _ r) = l <> r

-- | A 'View' of the head 'Ent' in the 'Balanced'.  Is 'Nothing' if the
-- 'Balanced' is empty.
viewL :: Balanced a -> Maybe (View a)
viewL (Balanced es) = case S.viewl es of
  EmptyL -> Nothing
  x :< xs -> Just $ View S.empty x xs

-- | A 'View' of the last 'Ent' in the 'Balanced'.  Is 'Nothing' if the
-- 'Balanced' is empty.
viewR :: Balanced a -> Maybe (View a)
viewR (Balanced es) = case S.viewr es of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x S.empty

-- | A single 'View' for each 'Ent' in the 'Balanced'.
allViews :: Balanced a -> Seq (View a)
allViews (Balanced sq) = go S.empty sq
  where
    go l curr = case S.viewl curr of
      EmptyL -> S.empty
      x :< xs -> View l x xs <| go (x <| l) xs


-- | Recreate the 'Balanced' from a 'View'.
unView :: View a -> Balanced a
unView (View l c r) = Balanced (l <> S.singleton c <> r)

-- | Get a new 'View' of the 'Ent' to the left of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the leftmost
-- 'Ent'.
moveLeft :: View a -> Maybe (View a)
moveLeft (View l c r) = case S.viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x (c <| r)

-- | Get a new 'View' of the 'Ent' to the right of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the
-- rightmost 'Ent'.
moveRight :: View a -> Maybe (View a)
moveRight (View l c r) = case S.viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ View (l |> c) x xs

-- | Returns a 'View' whose 'viewCurrent' 'Ent' has been transformed
-- by the given function.
changeCurrent :: (Ent a -> Ent a) -> View a -> View a
changeCurrent f (View l c r) = View l (f c) r

-- | Returns a 'View' where the metadata in the 'viewCurrent' 'Ent'
-- has been transformed by the given function.
changeCurrentMeta :: (a -> a) -> View a -> View a
changeCurrentMeta f (View l c r) = View l (fmap f c) r
