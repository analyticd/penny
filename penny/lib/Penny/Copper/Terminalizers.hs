{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Penny.Copper.Terminalizers where

import Penny.Copper.Grammar
import qualified Penny.Copper.Types as Types

import Pinchot (terminalizers)

$(terminalizers "Types" allRules)
