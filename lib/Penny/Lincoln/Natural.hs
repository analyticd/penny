module Penny.Lincoln.Natural
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

  -- * Length
  , length

  -- * Conversions
  , positiveToUnsigned
  , unsignedToPositive
  , novDecsToPositive
  ) where

import Penny.Lincoln.Natural.Internal
import Prelude hiding (length)
