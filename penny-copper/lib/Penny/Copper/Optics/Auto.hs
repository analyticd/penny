{-# LANGUAGE TemplateHaskell #-}
-- | Optics for the @lens@ library that correspond to the types in
-- "Penny.Copper.Types" are in this module.  These are auto-generated
-- by Pinchot.
module Penny.Copper.Optics.Auto where

import Penny.Copper.Types
import Penny.Copper.Grammar
import Pinchot (rulesToOptics)

$(rulesToOptics "" ''Char allRules)
