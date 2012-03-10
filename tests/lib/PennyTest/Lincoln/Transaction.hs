module PennyTest.Lincoln.Transaction where

import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Bits as B

import Control.Applicative (pure, (<$>), (<*>))
import Data.Text (pack)
import Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary, arbitrary)

-- | Given an Entry, make some other postings that will leave this
-- Entry inferable.
inferable :: B.Entry -> Gen [B.Entry]
inferable = undefined
