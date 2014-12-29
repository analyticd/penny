module Penny.Lincoln.Field where

import Data.Text
import Data.Sequence (Seq)
import Penny.Lincoln.Decimal
import Penny.Lincoln.DateTime

newtype Label = Label Text
  deriving (Eq, Ord, Show)

data Payload
  = PayDate DateTime
  | PayText Text
  | PayTexts (Seq Text)
  | PayDecimal Decimal
  deriving (Eq, Ord, Show)

data Field
  = Field Label Payload
  -- ^ @Field a b@, where
  --
  -- @a@ is the /label/ for the field, and
  --
  -- @b@ is the 'Payload', the data the field contains.
  deriving (Eq, Ord, Show)
