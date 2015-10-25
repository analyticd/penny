{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Penny.Qty where

import Control.Lens
import Penny.Decimal

newtype Qty = Qty (Exponential Integer)

makeWrapped ''Qty
