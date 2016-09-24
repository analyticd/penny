{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | The Earley grammar for every production in
-- "Penny.Copper.Grammar".
module Penny.Copper.EarleyGrammar where

import qualified Penny.Copper.Productions as Productions
import qualified Penny.Copper.Types as Types
import Penny.Copper.Grammar

import qualified Pinchot
import qualified Text.Earley as Earley

-- | This grammar contains every production in "Penny.Copper.Grammar".
-- Typically you will want to use 'fmap' to get the single production
-- you're interested in.
earleyGrammar :: Earley.Grammar r (Productions.Productions r Char a)
earleyGrammar = $(Pinchot.earleyProduct "Types" "Productions" allRules)
