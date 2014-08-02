-- | Balanced sets.  This module is the guardian of the core principle
-- of double-entry accounting, which is that all transactions must be
-- balanced.
module Penny.Balanced
  ( -- * Balanced
    Balanced
  , balanced
  , unBalanced
  , sequenceR
  , mapV

  -- * Views
  , View
  , unView
  , viewLeft
  , viewCurrent
  , viewRight
  , viewBalance
  , siblings
  , viewL
  , viewR
  , allViews
  , viewToBalanced
  , moveLeft
  , moveRight
  , changeCurrent
  , changeCurrentMeta
  ) where

import Control.Applicative
import Data.Sequence (Seq)
import qualified Penny.Ents as E
import Penny.Ents (Ent)
import qualified Data.Map as M
import Penny.Balance
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr

newtype Balanced m = Balanced { unBalanced :: E.Ents m }
  deriving (Eq, Ord, Show)

instance Functor Balanced where
  fmap f (Balanced es) = Balanced $ fmap f es

instance Monoid (Balanced m) where
  mempty = Balanced mempty
  mappend (Balanced x) (Balanced y) = Balanced $ x <> y

instance F.Foldable Balanced where
  foldr f z (Balanced es) = F.foldr f z es

instance Tr.Traversable Balanced where
  traverse f (Balanced es) = fmap Balanced (Tr.traverse f es)

-- | 'sequence' in reverse; that is, run each action in a 'Balanced'
-- from right to left.

sequenceR :: Applicative f => Balanced (f a) -> f (Balanced a)
sequenceR (Balanced es) = fmap Balanced (E.sequenceR es)

-- | Creates a 'Balanced' but only if the 'E.Ents' is balanced.
balanced :: E.Ents m -> Either Imbalances (Balanced m)
balanced e
  | not . M.null . unImbalances $ imbs = Left imbs
  | otherwise = Right $ Balanced e
  where
    imbs = onlyUnbalanced . E.entsBal $ e

-- | Change the metadata on each 'E.Ent', using a function that has a
-- view of the entire 'E.Ent'.  Like 'fmap' but gives a view of the
-- whole 'E.Ent' rather than just the metadata.
mapV :: (E.Ent a -> b) -> Balanced a -> Balanced b
mapV f = Balanced . E.mapV f . unBalanced

-- | A single 'E.Ent' with information about how it relates to
-- surrounding 'E.Ent' in the 'Balanced'.
newtype View a = View { unView :: E.View a }
  deriving (Eq, Ord, Show)

-- | These 'Ent' are to the left of the 'viewCurrent' 'Ent'.  Closer
-- 'Ent' are at the right end of the 'Seq'.
viewLeft :: View a -> Seq (Ent a)
viewLeft = E.viewLeft . unView

-- | The 'Ent' you are currently viewing
viewCurrent :: View a -> Ent a
viewCurrent = E.viewCurrent . unView

-- | These 'Ent' are to the right of the 'viewCurrent' 'Ent'.
-- Closer 'Ent' are at the left end of the 'Seq'.
viewRight :: View a -> Seq (Ent a)
viewRight = E.viewRight . unView

-- | The balance of all 'Ent' in the 'View'.
viewBalance :: View a -> Balances
viewBalance = E.viewBalance . unView

instance Functor View where
  fmap f = View . fmap f . unView

instance F.Foldable View where
  foldr f z = F.foldr f z . unView

instance Tr.Traversable View where
  sequenceA = fmap View . Tr.sequenceA . unView

-- | All 'Ent' that neighbor the 'viewCurrent' 'Ent'.  The list is
-- ordered with all 'Ent' from left to right; the 'viewCurrent' 'Ent'
-- is not in the resulting list.
siblings :: View a -> Seq (E.Ent a)
siblings = E.siblings . unView

-- | A 'View' of the head 'Ent' in the 'Balanced'.  Is 'Nothing' if
-- the 'Balanced' is empty.
viewL :: Balanced a -> Maybe (View a)
viewL = fmap View . E.viewL . unBalanced

-- | A 'View' of the last 'Ent' in the 'Balanced'.  Is 'Nothing' if
-- the 'Balanced is empty.
viewR :: Balanced a -> Maybe (View a)
viewR = fmap View . E.viewR . unBalanced

-- | A single 'View' for each 'Ent' in the 'Balanced'.
allViews :: Balanced a -> Seq (View a)
allViews = fmap View . E.allViews . unBalanced

-- | Recreate the 'Balanced' from the 'View'.
viewToBalanced :: View a -> Balanced a
viewToBalanced = Balanced . E.unView . unView

-- | Get a new 'View' of the 'Ent' to the left of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the leftmost
-- 'Ent'.
moveLeft :: View a -> Maybe (View a)
moveLeft = fmap View . E.moveLeft . unView

-- | Get a new 'View' of the 'Ent' to the right of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the
-- rightmost 'Ent'.
moveRight :: View a -> Maybe (View a)
moveRight = fmap View . E.moveRight . unView

-- | Returns a 'View' whose 'viewCurrent' 'Ent' has been transformed
-- by the given function.
changeCurrent :: (E.Ent a -> E.Ent a) -> View a -> View a
changeCurrent f = View . E.changeCurrent f . unView

-- | Returns a 'View' where the metadata in the 'viewCurrent' 'Ent'
-- has been transformed by the given function.
changeCurrentMeta :: (a -> a) -> View a -> View a
changeCurrentMeta f = View . E.changeCurrentMeta f . unView
