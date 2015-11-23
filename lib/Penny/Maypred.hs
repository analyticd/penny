{-# LANGUAGE RankNTypes #-}
module Penny.Maypred where

import Control.Lens
import Data.Time (fromGregorian)

import Penny.Clatch
import Penny.Shortcut

predicate :: Getter a (Maybe b) -> (b -> Bool) -> Getter a Bool
predicate getter pd = to f
  where
    f a = case view getter a of
      Nothing -> False
      Just b -> pd b

gt :: Ord b => Getter a (Maybe b) -> b -> Getter a Bool
gt getter x = predicate getter (> x)

lt :: Ord b => Getter a (Maybe b) -> b -> Getter a Bool
lt getter x = predicate getter (< x)

eq :: Ord b => Getter a (Maybe b) -> b -> Getter a Bool
eq getter x = predicate getter (== x)

gtEq :: Ord b => Getter a (Maybe b) -> b -> Getter a Bool
gtEq getter x = predicate getter (>= x)

ltEq :: Ord b => Getter a (Maybe b) -> b -> Getter a Bool
ltEq getter x = predicate getter (<= x)

on :: Integer -> Int -> Int -> Getter (Transaction, a) Bool
on yr mo day = eq date (fromGregorian yr mo day)

before :: Integer -> Int -> Int -> Getter (Transaction, a) Bool
before yr mo day = lt date (fromGregorian yr mo day)

after :: Integer -> Int -> Int -> Getter (Transaction, a) Bool
after yr mo day = gt date (fromGregorian yr mo day)

onOrAfter :: Integer -> Int -> Int -> Getter (Transaction, a) Bool
onOrAfter yr mo day = gtEq date (fromGregorian yr mo day)

onOrBefore :: Integer -> Int -> Int -> Getter (Transaction, a) Bool
onOrBefore yr mo day = ltEq date (fromGregorian yr mo day)
