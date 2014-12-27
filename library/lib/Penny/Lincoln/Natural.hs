module Penny.Lincoln.Natural
  ( Natural(..)
  , NonZero
  , Unsigned

  -- * Constants
  --
  -- | For additional constants, use the
  -- 'Penny.Lincoln.Digits.OneToNine' and 'Penny.Lincoln.Digits.Zero'
  -- instances.
  , ten

  -- * Arithmetic
  -- | For additional arithmetic, use the 'Natural' methods.
  , addNonZeroToUnsigned
  , addUnsignedToNonZero
  , monus
  , subt
  , pow
  , divide

  -- * Length
  , length

  -- * Conversions
  , nonZeroToUnsigned
  , unsignedToNonZero
  ) where

import Penny.Lincoln.Natural.Internal
import Prelude hiding (length)
