{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Copper.Types where

import Data.Data (Data)
import Data.Monoid ((<>))
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
  mempty = WholeFile (FileItemsP'Opt Nothing) mempty
  mappend (WholeFile (FileItemsP'Opt Nothing) whites0)
          (WholeFile (FileItemsP'Opt Nothing) whites1)
          = WholeFile (FileItemsP'Opt Nothing) (whites0 <> whites1)

  mappend
    (WholeFile (FileItemsP'Opt (Just
      (FileItemsP leftFiWhites leftItems))) leftWfWhites)
    (WholeFile (FileItemsP'Opt Nothing) rightWfWhites)
    = WholeFile (FileItemsP'Opt (Just
      (FileItemsP leftFiWhites leftItems))) (leftWfWhites <> rightWfWhites)

  mappend
    (WholeFile (FileItemsP'Opt Nothing) leftWfWhites)
    (WholeFile (FileItemsP'Opt (Just
      (FileItemsP rightFiWhites rightItems))) rightWfWhites)
    = WholeFile (FileItemsP'Opt (Just
      (FileItemsP (leftWfWhites <> rightFiWhites) rightItems))) rightWfWhites

  mappend
    (WholeFile (FileItemsP'Opt (Just
      (FileItemsP leftFiWhites leftItems))) leftWfWhites)
    (WholeFile (FileItemsP'Opt (Just
      (FileItemsP rightFiWhites rightItems))) rightWfWhites)
    = WholeFile (FileItemsP'Opt (Just
      (FileItemsP fiWhites' fileItems'))) wfWhites'
    where
      fiWhites' = leftFiWhites
      fileItems' = FileItems firstItem restItems
        where
          FileItems firstItem (FileItemP'Star leftRestItems) = leftItems
          FileItems firstRightItem (FileItemP'Star rightRestItems) = rightItems
          restItems = FileItemP'Star (leftRestItems <>
            (firstRightItemWithWhites <| rightRestItems))
          firstRightItemWithWhites = FileItemP (leftWfWhites <> rightFiWhites)
            firstRightItem
      wfWhites' = rightWfWhites

