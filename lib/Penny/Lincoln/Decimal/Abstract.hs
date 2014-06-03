module Penny.Lincoln.Decimal.Abstract where

import Penny.Lincoln.Decimal.Zero
import Penny.Lincoln.Decimal.Whole
import Penny.Lincoln.Decimal.Frac
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Decimal.Lane
import Penny.Lincoln.Decimal.Side
import Prelude hiding (exponent)

data NonZero
  = NZWhole Whole
  | NZFrac Frac
  deriving (Eq, Ord, Show)

instance HasDecuple NonZero where
  decuple z = case z of
    NZWhole w -> decuple w
    NZFrac f -> decuple f

instance HasCoefficient NonZero where
  coefficient z = case z of
    NZWhole w -> coefficient w
    NZFrac f -> coefficient f

instance HasExponent NonZero where
  exponent z = case z of
    NZWhole w -> exponent w
    NZFrac f -> exponent f

data Figure = Figure
  { figSide :: Side
  , figNonZero :: NonZero
  } deriving (Eq, Ord, Show)

instance Sided Figure where
  side = figSide

instance Laned Figure where
  lane f = NonCenter (figSide f, decuple . figNonZero $ f)

instance HasDecuple Figure where
  decuple = decuple . figNonZero

instance HasCoefficient Figure where
  coefficient = coefficient . figNonZero

instance HasExponent Figure where
  exponent = exponent . figNonZero

data Rep
  = RFigure Figure
  | RZero Zero
  deriving (Eq, Ord, Show)

instance Laned Rep where
  lane r = case r of
    RFigure f -> lane f
    RZero _ -> Center

instance HasCoefficient Rep where
  coefficient a = case a of
    RFigure f -> coefficient f
    RZero z -> coefficient z

instance HasExponent Rep where
  exponent a = case a of
    RFigure f -> exponent f
    RZero z -> exponent z

data RadGroup
  = PeriodComma
  | PeriodSpace
  | PeriodThinSpace
  | CommaPeriod
  | CommaSpace
  | CommaThinSpace
  deriving (Eq, Ord, Show)

data Abstract = Abstract
  { absRep :: Rep
  , absRadGroup :: RadGroup
  } deriving (Eq, Ord, Show)

instance Laned Abstract where
  lane = lane . absRep

instance HasCoefficient Abstract where
  coefficient = coefficient . absRep

instance HasExponent Abstract where
  exponent = exponent . absRep
