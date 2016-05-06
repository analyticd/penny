{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Ents
  ( Ents
  , entsToSeqEnt
  , entsToImbalance
  , appendEnt
  , prependEnt
  , appendTrio
  , prependTrio
  , Balanced
  , balancedToSeqEnt
  , entsToBalanced
  , ImbalancedError(..)
  ) where

import Control.Lens ((<|), (|>))
import Data.Sequence (Seq, viewl, ViewL(EmptyL, (:<)))
import qualified Data.Sequence as S
import Data.Monoid ((<>))
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Text as X
import qualified Data.Map as M

import Penny.Amount
import Penny.Balance
import Penny.Commodity
import Penny.Copper.Decopperize (trioToTroiload, c'Amount'Troika)
import Penny.Decimal
import Penny.Friendly
import Penny.NonZero
import Penny.Trio
import qualified Penny.Troika as Y

data Ents a = Ents
  { entsToSeqEnt :: Seq (Y.Troika, a)
  , entsToImbalance :: Imbalance
  } deriving Show

instance Functor Ents where
  fmap f (Ents s b) = Ents (fmap (fmap f) s) b

instance Monoid (Ents m) where
  mempty = Ents mempty mempty
  mappend (Ents s1 b1) (Ents s2 b2) = Ents (s1 <> s2) (b1 <> b2)

instance Foldable Ents where
  foldr f z = F.foldr f z . fmap snd . entsToSeqEnt

instance Traversable Ents where
  sequenceA (Ents sq bl) = fmap (flip Ents bl) . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs

newtype Balanced a = Balanced { balancedToSeqEnt :: Seq (Y.Troika, a) }
  deriving Show

instance Functor Balanced where
  fmap f (Balanced sq) = Balanced $ fmap (fmap f) sq

instance Monoid (Balanced a) where
  mempty = Balanced mempty
  mappend (Balanced x) (Balanced y) = Balanced (x <> y)

instance Foldable Balanced where
  foldr f z = F.foldr f z . fmap snd . balancedToSeqEnt

instance Traversable Balanced where
  sequenceA (Balanced sq) = fmap Balanced . go $ sq
    where
      go es = case viewl es of
        EmptyL -> pure S.empty
        e :< xs ->
          (<|) <$> T.sequenceA e <*> go xs

appendEnt :: Ents a -> (Amount, a) -> Ents a
appendEnt (Ents s b) (am@(Amount cy q), e) = Ents (s |> (tm, e))
  (b <> c'Imbalance'Amount am)
  where
    tm = Y.Troika cy (Right q)

prependEnt :: (Amount, a) -> Ents a -> Ents a
prependEnt (am@(Amount cy q), e) (Ents s b) = Ents ((tm, e) <| s)
  (b <> c'Imbalance'Amount am)
  where
    tm = Y.Troika cy (Right q)

appendTrio :: Ents a -> Trio -> Either TrioError (a -> Ents a)
appendTrio (Ents sq imb) trio = fmap f $ trioToTroiload imb trio
  where
    f (troiload, cy) = \meta -> Ents (sq |> (tm, meta)) imb'
      where
        tm = Y.Troika cy (Left troiload)
        imb' = imb <> c'Imbalance'Amount (c'Amount'Troika tm)

prependTrio :: Ents a -> Trio -> Either TrioError (a -> Ents a)
prependTrio (Ents sq imb) trio = fmap f $ trioToTroiload imb trio
  where
    f (troiload, cy) = \meta -> Ents ((tm, meta) <| sq) imb'
      where
        tm = Y.Troika cy (Left troiload)
        imb' = imb <> c'Imbalance'Amount (c'Amount'Troika tm)


data ImbalancedError
  = ImbalancedError (Commodity, DecNonZero) [(Commodity, DecNonZero)]
  deriving Show

instance Friendly ImbalancedError where
  friendly (ImbalancedError c1 cs) =
    [ "Transaction is not balanced.  Imbalance:"
    , showImb c1
    ] ++ map showImb cs
    where
      showImb (cy, q)
        = "  " ++ X.unpack cy ++ " "
               ++ displayDecimalAsQty (fmap c'Integer'NonZero q) ""

entsToBalanced :: Ents a -> Either ImbalancedError (Balanced a)
entsToBalanced (Ents sq (Imbalance m)) = case M.toList m of
  [] -> return $ Balanced sq
  x:xs -> Left $ ImbalancedError x xs

