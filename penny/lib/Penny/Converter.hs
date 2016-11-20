{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Penny.Converter where

import Control.Lens
import Control.Applicative

import Penny.Amount
import Penny.Commodity
import Penny.Decimal
import Penny.NonNegative

-- | A function that converts one 'Amount' to another.
newtype Converter = Converter (Amount -> Maybe Amount)

makeWrapped ''Converter

-- | For a given 'Amount', 'mappend' uses the first 'Converter' that
-- succeeds, or performs no conversion if neither 'Converter' performs
-- a conversion.  'mempty' performs no conversion.
instance Monoid Converter where
  mempty = Converter (const Nothing)
  mappend (Converter x) (Converter y) = Converter $ \a -> x a <|> y a

-- | Converts one commodity to another, using a particular conversion
-- factor.
convert
  :: Commodity
  -- ^ Convert from this commodity
  -> Commodity
  -- ^ Convert to this commodity
  -> DecUnsigned
  -> Converter
convert fromCy toCy uns = Converter fn
  where
    fn (Amount oldCy oldQty)
      | oldCy /= fromCy = Nothing
      | otherwise = Just $ Amount toCy (oldQty * factor)
    factor = fmap c'Integer'NonNegative uns
