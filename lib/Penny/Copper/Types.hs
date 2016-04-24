{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Copper.Types where

import Penny.Copper.Grammar

import Pinchot (syntaxTrees, wrappedInstances)

syntaxTrees ''Char [''Eq, ''Ord, ''Show, ''Functor, ''Foldable, ''Traversable]
  [wholeFile, nilOrBrimRadCom, nilOrBrimRadPer]

wrappedInstances
  [wholeFile, nilOrBrimRadCom, nilOrBrimRadPer]
