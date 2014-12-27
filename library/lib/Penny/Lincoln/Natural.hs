module Penny.Lincoln.Natural
  ( Natural(..)
  , Positive
  , Unsigned

  -- * Constants
  --
  -- | For additional constants, use the
  -- 'Penny.Lincoln.Digits.OneToNine' and 'Penny.Lincoln.Digits.Zero'
  -- instances.
  , ten

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
  ) where

import Penny.Lincoln.Natural.Internal
import Prelude hiding (length)
