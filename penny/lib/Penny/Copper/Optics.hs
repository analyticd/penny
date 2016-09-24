{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | Optics for the @lens@ library that correspond to the types in
-- "Penny.Copper.Types" are in this module.
module Penny.Copper.Optics
  ( module Penny.Copper.Optics.Auto
  , module Penny.Copper.Optics.Manual
  ) where

import Penny.Copper.Optics.Auto
import Penny.Copper.Optics.Manual
