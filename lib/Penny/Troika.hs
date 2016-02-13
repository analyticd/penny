{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Penny.Troika where

import Penny.Arrangement
import Penny.Commodity
import Penny.Decimal
import Penny.NonZero
import Penny.Polar
import Penny.Rep
import qualified Penny.Amount as A

import Control.Lens
import Data.Sequence (Seq)

data Troiload
  = QC RepAnyRadix Arrangement
  | Q RepAnyRadix
  | SC DecNonZero
  | S DecNonZero
  | UC BrimScalarAnyRadix Pole Arrangement
  | U BrimScalarAnyRadix Pole
  | C DecNonZero
  | E DecNonZero
  deriving Show

instance Equatorial Troiload where
  equatorial x = case x of
    QC q _ -> either equatorial equatorial q
    Q q -> either equatorial equatorial q
    SC qnz -> Just . polar $ qnz
    S qnz -> Just . polar $ qnz
    UC _ s _ -> Just s
    U _ s -> Just s
    C qnz -> Just . polar $ qnz
    E qnz -> Just . polar $ qnz

instance HasDecimal Troiload where
  toDecimal x = case x of
    QC q _ -> toDecimal q
    Q q -> toDecimal q
    SC qnz -> fmap nonZeroToInteger qnz
    S qnz -> fmap nonZeroToInteger qnz
    UC rnn s _ ->
      fmap nonZeroToInteger
      . c'DecNonZero'DecPositive s
      . toDecPositive
      $ rnn
    U rnn s ->
      fmap nonZeroToInteger
      . c'DecNonZero'DecPositive s
      . toDecPositive
      $ rnn
    C qnz -> fmap nonZeroToInteger qnz
    E qnz -> fmap nonZeroToInteger qnz

type Troiquant = Either Troiload Decimal

data Troika = Troika
  { _commodity :: Commodity
  , _troiquant :: Either Troiload Decimal
  } deriving Show

makeLenses ''Troika

instance HasDecimal Troika where
  toDecimal (Troika _ tq) = toDecimal tq

c'Amount'Troika :: Troika -> A.Amount
c'Amount'Troika (Troika cy ei) = A.Amount cy (toDecimal ei)

c'Troika'Amount :: A.Amount -> Troika
c'Troika'Amount (A.Amount cy q) = Troika cy (Right q)

troikaRendering
  :: Troika
  -> Maybe (Commodity, Arrangement, Either (Seq RadCom) (Seq RadPer))
troikaRendering (Troika cy tq) = case tq of
  Left tl -> case tl of
    QC qr ar -> Just (cy, ar, ei)
      where
        ei = case qr of
          Left (Moderate n) -> Left . mayGroupers $ n
          Left (Extreme (Polarized o _)) -> Left . mayGroupers $ o
          Right (Moderate n) -> Right . mayGroupers $ n
          Right (Extreme (Polarized o _)) -> Right . mayGroupers $ o
    UC rnn _ ar -> Just (cy, ar, ei)
      where
        ei = case rnn of
          Left b -> Left $ mayGroupers b
          Right b -> Right $ mayGroupers b
    _ -> Nothing
  _ -> Nothing
