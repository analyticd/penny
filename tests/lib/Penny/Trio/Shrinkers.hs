{-# LANGUAGE NoImplicitPrelude #-}

module Penny.Trio.Shrinkers where

import Penny.Trio
import Penny.Numbers.Abstract.Aggregates.Shrinkers
import Prelude.Shrinkers
import Penny.Common.Shrinkers
import Prelude
import Penny.Numbers.Qty.Shrinkers

trio :: Trio -> [Trio]
trio tri = case tri of
  QC e cy ar ->
    [ QC e' cy' ar' | (e', cy', ar') <- tuple3
      polarEitherRadix commodity arrangement (e, cy, ar) ]

  Q e -> fmap Q $ polarEitherRadix e

  SC s c -> [ SC s' c' | (s', c') <- tuple2 side commodity (s, c) ]

  S s -> fmap S $ side s

  UC e c a ->
    [ UC e' c' a' | (e', c', a') <-
      tuple3 unpolarEitherRadix commodity arrangement (e, c, a) ]

  U e -> fmap U $ unpolarEitherRadix e

  C c -> fmap C $ commodity c

  E -> []
