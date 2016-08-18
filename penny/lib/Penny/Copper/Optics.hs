{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Penny.Copper.Optics where

import qualified Penny.Copper.Types as Types
import Penny.Copper.Grammar
import Pinchot (rulesToOptics)

$(rulesToOptics "Types" ''Char allRules)
