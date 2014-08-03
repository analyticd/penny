-- | Sets of entries.  Though every 'Ent' in an 'Ents' is valid, the
-- entire set is not necessarily balanced; for balanced sets, see
-- "Penny.Balanced".
module Penny.Ents
  (
  -- * Ent
    Ent
  , entQty
  , entCommodity
  , entTrio
  , entMeta
  , Entrio(..)
  , entToTrio
  , rearrange

  -- * Ents
  , Ents
  , entsSeq
  , entsBal
  , appendEnt
  , rEnts
  , mapV
  , sequence
  , sequenceR

  -- * Errors
  , EntError(..)
  , ErrorCode(..)

  -- * Views
  , View
  , viewL
  , viewR
  , allViews
  , unView
  , viewLeft
  , viewCurrent
  , viewRight
  , viewBalance
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
import qualified Penny.Trio as T
import qualified Data.Map as M
import Penny.Numbers.Concrete
import Prelude hiding (negate, sequence)
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import Data.Either.Combinators
import Data.Sequence (Seq, (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import qualified Data.Foldable as F
import Control.Applicative

-- | Zero or more 'Ent'.  Together they are balanced; that is, every
-- 'Debit' of a particular 'Commodity' is offset by an equal number of
-- 'Credit's of the same 'Commodity', and vice versa.
--
-- 'Ents' is a 'Monoid'.
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
  sequenceA = sequence

-- | Run each action in an 'Ents' from left to right.
sequence :: Applicative f => Ents (f a) -> f (Ents a)
sequence (Ents sq bl) = fmap (flip Ents bl) . go $ sq
  where
    go es = case S.viewl es of
      EmptyL -> pure S.empty
      (Ent q c e m) :< xs ->
        (<|) <$> (Ent q c e <$> m)
             <*> go xs

-- | 'sequence' in reverse; that is, run each action in an 'Ents' from
-- right to left.
sequenceR :: Applicative f => Ents (f a) -> f (Ents a)
sequenceR (Ents sq bl) = fmap (flip Ents bl) . go $ sq
  where
    go es = case S.viewr es of
      EmptyR -> pure S.empty
      xs :> (Ent q c e m) ->
        (flip (|>)) <$> (Ent q c e <$> m) <*> go xs

-- | Change the metadata on each 'Ent', using a function that has a
-- view of the entire 'Ent'.  Like 'fmap' but gives a view of the
-- whole 'Ent' rather than just the metadata.
mapV :: (Ent a -> b) -> Ents a -> Ents b
mapV f (Ents sq bl) = flip Ents bl . go $ sq
  where
    go s = case S.viewl s of
      EmptyL -> S.empty
      w@(Ent q c e _) :< xs -> Ent q c e (f w) <| go xs


-- | Change the 'Arrangement' in an 'Ent'.  Does nothing if the 'Ent'
-- has no 'Arrangement' to begin with.
rearrange :: Arrangement -> Ent m -> Ent m
rearrange a' e = e { entTrio = e' }
  where
    e' = case entTrio e of
      QC a _ -> QC a a'
      UC b _ -> UC b a'
      x -> x

procEnt
  :: Balances
  -> (T.Trio, m)
  -> Either EntError (Balances, Ent m)
procEnt bals (tri, mta) = fmap f $ procTrio unbals tri
  where
    unbals = onlyUnbalanced bals
    f (q, cy) = (bals', ent)
      where
        ent = Ent q cy (buildEntrio tri) mta
        bals' = bals <> balance cy q


appendEnt
  :: Ents m
  -> (T.Trio, m)
  -> Either EntError (Ents m)
appendEnt (Ents s b) p = case procEnt b p of
  Left e -> Left e
  Right (b', e) -> Right $ Ents (s |> e) b'


-- | Creates 'Ents' but, unlike 'appendEnt', never fails.  To make this
-- guarantee, 'rEnts' puts restrictions on its arguments.

rEnts
  :: Side
  -- ^ All postings that you supply are on this 'Side'

  -> Commodity
  -- ^ All postings will have this same 'Commodity'

  -> Arrangement
  -- ^ All postings except for the inferred one will use this
  -- 'Arrangement'

  -> a
  -- ^ Metadata for inferred posting

  -> Seq (Either (Unpolar Period) (Unpolar Comma), a)
  -- ^ Each non-zero member, and its corresponding metadata

  -> Ents a
  -- ^ The resulting 'Ents'.  If the list of 'NZGrouped' was
  -- non-empty, then a single inferred posting is at the end to
  -- balance out the other non-zero postings.  Each 'Ent' has an
  -- 'Entrio' of 'SZC', except for the inferred posting, which is 'E'.

rEnts s cy ar mt ls
  | S.null ls = Ents mempty mempty
  | otherwise = Ents sq bl
  where
    sq = S.zipWith mkEnt qs ls |> inferred
    bl = mconcat . F.toList . fmap (uncurry balance)
      . fmap (\e -> (entCommodity e, entQty e)) $ sq
    qs = fmap (mkQ . fst) ls
    mkQ = either (unpolarToQty s) (unpolarToQty s)
    offset = Qty . negate . F.sum . fmap unQty $ qs
    inferred = Ent offset cy E mt
    mkEnt q (ei, m) = Ent q cy (QC ei' ar) m
      where
        ei' = mapBoth (polarizeUnpolar s) (polarizeUnpolar s) ei

-- | A single 'Ent' with information about how it relates to
-- surrounding 'Ent' in the 'Ents'.
data View a = View
  { viewLeft :: Seq (Ent a)
  -- ^ These 'Ent' are to the left of the 'viewCurrent' 'Ent'.  Closer
  -- 'Ent' are at the right end of the 'Seq'.

  , viewCurrent :: Ent a
  -- ^ The 'Ent' you are currently viewing

  , viewRight :: Seq (Ent a)
  -- ^ These 'Ent' are to the right of the 'viewCurrent' 'Ent'.
  -- Closer 'Ent' are at the left end of the 'Seq'.

  , viewBalance :: Balances
  -- ^ The balance of all 'Ent' in the 'View'.

  } deriving (Eq, Ord, Show)

instance Functor View where
  fmap f (View l c r b) = View (fmap (fmap f) l) (fmap f c)
    (fmap (fmap f) r) b

instance F.Foldable View where
  foldr f z (View l c r _) = F.foldr f z
    (fmap entMeta $ l <> S.singleton c <> r)

instance Tr.Traversable View where
  sequenceA v = View
    <$> trSeq (viewLeft v)
    <*> trEnt (viewCurrent v)
    <*> trSeq (viewRight v)
    <*> pure (viewBalance v)
    where
      trEnt (Ent q c e m) = Ent q c e <$> m
      trSeq sq = case S.viewl sq of
        EmptyL -> pure S.empty
        e :< es -> (<|) <$> trEnt e <*> trSeq es

-- | All 'Ent' that neighbor the 'viewCurrent' 'Ent'.  The list is
-- ordered with all 'Ent' from left to right; the 'viewCurrent' 'Ent'
-- is not in the resulting list.
siblings :: View a -> Seq (Ent a)
siblings (View l _ r _) = l <> r

-- | A 'View' of the head 'Ent' in the 'Ents'.  Is 'Nothing' if the
-- 'Ents' is empty.
viewL :: Ents a -> Maybe (View a)
viewL (Ents es b) = case S.viewl es of
  EmptyL -> Nothing
  x :< xs -> Just $ View S.empty x xs b

-- | A 'View' of the last 'Ent' in the 'Ents'.  Is 'Nothing' if the
-- 'Ents is empty.
viewR :: Ents a -> Maybe (View a)
viewR (Ents es b) = case S.viewr es of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x S.empty b

-- | A single 'View' for each 'Ent' in the 'Ents'.
allViews :: Ents a -> Seq (View a)
allViews (Ents sq b) = go S.empty sq
  where
    go l curr = case S.viewl curr of
      EmptyL -> S.empty
      x :< xs -> View l x xs b <| go (x <| l) xs


-- | Recreate the 'Ents' from a 'View'.
unView :: View a -> Ents a
unView (View l c r b) = Ents (l <> S.singleton c <> r) b

-- | Get a new 'View' of the 'Ent' to the left of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the leftmost
-- 'Ent'.
moveLeft :: View a -> Maybe (View a)
moveLeft (View l c r b) = case S.viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x (c <| r) b

-- | Get a new 'View' of the 'Ent' to the right of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the
-- rightmost 'Ent'.
moveRight :: View a -> Maybe (View a)
moveRight (View l c r b) = case S.viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ View (l |> c) x xs b

-- | Returns a 'View' whose 'viewCurrent' 'Ent' has been transformed
-- by the given function.
changeCurrent :: (Ent a -> Ent a) -> View a -> View a
changeCurrent f (View l c r b) = View l (f c) r b

-- | Returns a 'View' where the metadata in the 'viewCurrent' 'Ent'
-- has been transformed by the given function.
changeCurrentMeta :: (a -> a) -> View a -> View a
changeCurrentMeta f (View l c r b) = View l (fmap f c) r b
