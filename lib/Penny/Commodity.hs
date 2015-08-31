{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Penny.Commodity where

import Data.Text
import Control.Lens

newtype Commodity = Commodity Text
  deriving (Eq, Ord, Show)

makeWrapped ''Commodity
