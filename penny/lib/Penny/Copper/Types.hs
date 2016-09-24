{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | All the types corresponding to the grammar in
-- "Penny.Copper.Grammar".  Most of this is automatically generated by
-- Pinchot.
module Penny.Copper.Types where

import qualified Control.Lens as Lens
import Data.Data (Data)
import Data.Monoid ((<>))
import Penny.Copper.Grammar

import Pinchot (syntaxTrees, wrappedInstances,
 bifunctorInstances, semigroupInstances, monoidInstances, prettyInstances)

syntaxTrees [] allRules -- [''Eq, ''Ord, ''Show, ''Functor, ''Foldable, ''Traversable,
  -- ''Data] allRules

wrappedInstances allRules

bifunctorInstances allRules

semigroupInstances allRules

monoidInstances allRules

prettyInstances allRules

instance Monoid (WholeFile t a) where
  mempty = WholeFile mempty mempty
  mappend (WholeFile fiSqX wiSqX)
          (WholeFile (FileItemP'Star fiSqY) wiSqY)
    = WholeFile (fiSqX <> fiSq') (wiSq' <> wiSqY)
    where
      (fiSq', wiSq') = case Lens.uncons fiSqY of
        Nothing -> (mempty, wiSqX)
        Just (FileItemP whites fi, rest) ->
          ( FileItemP'Star (Lens.cons (FileItemP (wiSqX <> whites) fi) rest)
          , mempty )

