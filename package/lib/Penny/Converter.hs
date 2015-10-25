{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Penny.Converter where

import Control.Lens
import Penny.Amount
import Control.Applicative

-- | A function that converts one 'Amount' to another.
newtype Converter = Converter (Amount -> Maybe Amount)

makeWrapped ''Converter

-- | For a given 'Amount', 'mappend' uses the first 'Converter' that
-- succeeds, or performs no conversion if neither 'Converter' performs
-- a conversion.  'mempty' performs no conversion.
instance Monoid Converter where
  mempty = Converter (const Nothing)
  mappend (Converter x) (Converter y) = Converter $ \a -> x a <|> y a
