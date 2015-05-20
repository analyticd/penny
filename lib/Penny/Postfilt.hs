{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Penny.Postfilt where

import Control.Lens
import Penny.Converted
import Penny.Ledger
import Penny.SeqUtil
import Data.Foldable (Foldable)
import Penny.Transbox
import Penny.Prefilt
import Penny.Viewpost
import Penny.Sorted
import Penny.Filtered

-- | Input to the post-filter.
type Postfilt l a = Transbox l (Viewpost l (Converted (Filtered (Sorted a))))
