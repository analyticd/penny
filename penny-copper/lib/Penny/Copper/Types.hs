{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Copper.Types where

import Data.Data (Data)
import Penny.Copper.Grammar

import Pinchot (syntaxTrees, wrappedInstances,
 bifunctorInstances, semigroupInstances, monoidInstances)

syntaxTrees [''Eq, ''Ord, ''Show, ''Functor, ''Foldable, ''Traversable,
  ''Data] allRules

wrappedInstances allRules

bifunctorInstances allRules

semigroupInstances allRules

monoidInstances allRules
