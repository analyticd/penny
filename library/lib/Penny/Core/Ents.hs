module Penny.Core.Ents
  ( T
  , ents
  , bals
  , appendEnt
  , prependEnt
  , empty
  , appendTrio
  , prependTrio
  ) where

import Data.Sequence (Seq, viewl, ViewL(..), (<|), (|>))
import qualified Data.Sequence as S
import qualified Penny.Core.Balances as Bals
import qualified Penny.Core.Ent as Ent
import Data.Monoid
import Data.Foldable
import qualified Data.Foldable as F
import Data.Traversable
import Control.Applicative hiding (empty)
import qualified Penny.Core.Trio as Trio
import qualified Penny.Core.Trio.Error as Trio.Error
import qualified Penny.Core.Imbalances as Imbalances

-- | A collection of 'Penny.Core.Ent.T', along with the
-- 'Penny.Core.Balances.T' of those 'Penny.Core.Ent.T'.  The
-- collection is not necessarily balanced.
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
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> sequenceA e <*> go xs

appendEnt :: T a -> Ent.T a -> T a
appendEnt (T s b) e = T (s |> e)
  (b <> Bals.fromPair (Ent.commodity e) (Ent.qty e))

prependEnt :: Ent.T a -> T a -> T a
prependEnt e (T s b) = T (e <| s)
  (b <> Bals.fromPair (Ent.commodity e) (Ent.qty e))

empty :: T a
empty = T S.empty Bals.empty

appendTrio :: a -> T a -> Trio.T -> Either Trio.Error.T (T a)
appendTrio a t@(T _ bal) trio =
  case Trio.toEnt (Imbalances.fromBalances bal) trio a of
    Left e -> Left e
    Right e -> Right $ appendEnt t e

prependTrio :: a -> Trio.T -> T a -> Either Trio.Error.T (T a)
prependTrio a trio t@(T _ bal) =
  case Trio.toEnt (Imbalances.fromBalances bal) trio a of
    Left e -> Left e
    Right e -> Right $ prependEnt e t
