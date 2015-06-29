{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Penny.Prefilt where


import Control.Lens hiding (pre)
import Penny.Converted
import Penny.Ledger
import Penny.SeqUtil
import Data.Foldable (Foldable)
import Penny.Transbox
import Penny.Viewpost

-- | Input to the pre-filter.
type Prefilt a = Transbox (Viewpost (Converted a))

