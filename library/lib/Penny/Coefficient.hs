module Penny.Coefficient where

import qualified Deka.Dec as D
import qualified Penny.NovDecs as NovDecs
import qualified Deka.Native as DN

-- | Coefficients.  Unlike Deka coefficients, these carry the sign of
-- the number.

data T
  = Zero
  -- ^ All 'Zero' have a 'D.Sign' of 'D.Sign0'; that is, no
  -- negative zeroes are allowed.
  | NonZero NovDecs.T D.Sign
  deriving (Eq, Ord, Show)

fromDeka :: D.Sign -> DN.Coefficient -> T
fromDeka s (DN.Coefficient c) = case c of
  DN.Nil -> Zero
  DN.Plenus dc -> NonZero (NovDecs.fromDecuple dc) s

toDeka :: T -> (DN.Coefficient, D.Sign)
toDeka c = case c of
  Zero -> (DN.Coefficient DN.Nil, D.Sign0)
  NonZero nv sg ->
    (DN.Coefficient $ DN.Plenus (NovDecs.toDecuple nv), sg)
