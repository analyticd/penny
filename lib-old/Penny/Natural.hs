module Penny.Natural
  ( Natural(..)
  , Positive
  , Unsigned

  -- * Typeclasses
  --
  , IsPositive(..)
  , IsUnsigned(..)

  -- * Arithmetic
  -- | For additional arithmetic, use the 'Natural' methods.
  , addPositiveToUnsigned
  , addUnsignedToPositive
  , monus
  , subt
  , pow
  , divide
  , Differ(..)
  , diffUnsigned

  -- * Length
  , lengthUnsigned

  -- * Conversions
  , positiveToUnsigned
  , unsignedToPositive
  , novDecsToPositive
  , stripIntegerSign
  , positiveDigits
  ) where

import Penny.Natural.Internal
