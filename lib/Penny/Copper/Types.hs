{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Penny.Copper.Types where

import Penny.Copper.Grammar

import Pinchot (syntaxTrees, wrappedInstances)

syntaxTrees ''Char [''Eq, ''Ord, ''Show]
  [wholeFile, nilOrBrimRadCom, nilOrBrimRadPer]

wrappedInstances
  [wholeFile, nilOrBrimRadCom, nilOrBrimRadPer]
