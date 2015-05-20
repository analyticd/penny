{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Filtered where

import Control.Lens
import Penny.Serial
import Data.Foldable

newtype Filtered a = Filtered (Sersetted a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeWrapped ''Filtered
