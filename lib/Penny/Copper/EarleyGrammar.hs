{-# LANGUAGE TemplateHaskell #-}
module Penny.Copper.EarleyGrammar where

import qualified Penny.Copper.Productions as Productions
import qualified Penny.Copper.Types as Types
import Penny.Copper.Grammar

import qualified Pinchot
import qualified Text.Earley as Earley

earleyGrammar :: Earley.Grammar r (Productions.Productions r)
earleyGrammar = $(Pinchot.earleyProduct "Types" "Productions" grammar)
