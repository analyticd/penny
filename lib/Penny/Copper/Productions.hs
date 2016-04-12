{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Penny.Copper.Productions where

import Penny.Copper.Grammar
import qualified Penny.Copper.Types as Types

import Pinchot (allRulesRecord)

$(allRulesRecord "Types" ''Char [wholeFile])
