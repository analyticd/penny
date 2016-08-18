{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Copper.Types where

import Data.Data (Data)
import Data.Sequence (viewl, ViewL(EmptyL, (:<)), (<|))
import Penny.Copper.Grammar

import Pinchot (syntaxTrees, wrappedInstances,
 bifunctorInstances, semigroupInstances, monoidInstances, prettyInstances)

syntaxTrees [''Eq, ''Ord, ''Show, ''Functor, ''Foldable, ''Traversable,
  ''Data] allRules

wrappedInstances allRules

bifunctorInstances allRules

semigroupInstances allRules

monoidInstances allRules

prettyInstances allRules

instance Monoid (WholeFile t a) where
  mempty = WholeFile mempty mempty
  mappend (WholeFile (WhitesFileItem'Star i0) w0)
          (WholeFile (WhitesFileItem'Star i1) w1)
          = WholeFile (WhitesFileItem'Star i') w'
    where
      (i', w') = case viewl i1 of
        EmptyL -> (i0, w0 `mappend` w1)
        WhitesFileItem wfiWhites fi :< rest ->
          (i0 `mappend`
            (WhitesFileItem
              (w0 `mappend` wfiWhites) fi <| rest), w1)
