{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Penny.Troika where

import qualified Penny.Amount as A
import Penny.Arrangement
import Penny.Commodity
import Penny.Decimal
import Penny.Copper.Types (GrpRadCom, GrpRadPer)
import Penny.NonZero
import Penny.Polar
import Penny.Rep

import Control.Lens
import Data.Sequence (Seq)

data Troiload
  = QC RepAnyRadix Arrangement
  | Q RepAnyRadix
  | SC DecNonZero
  | S DecNonZero
  | UC BrimAnyRadix Pole Arrangement
  | NC NilAnyRadix Arrangement
  | US BrimAnyRadix Pole
  | UU NilAnyRadix
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
    NC _ _ -> Nothing
    US _ s -> Just s
    UU _ -> Nothing
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
    NC nilAnyRadix _ ->
      fmap (const 0) . toDecZero $ nilAnyRadix
    US rnn s ->
      fmap nonZeroToInteger
      . c'DecNonZero'DecPositive s
      . toDecPositive
      $ rnn
    UU nil -> fmap (const 0) . toDecZero $ nil
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
  -> Maybe (Commodity, Arrangement, Either (Seq GrpRadCom) (Seq GrpRadPer))
troikaRendering (Troika cy tq) = case tq of
  Left tl -> case tl of
    QC qr ar -> Just (cy, ar, ei)
      where
        ei = case qr of
          Left (Moderate n) -> Left . mayGroupersRadCom $ n
          Left (Extreme (Polarized o _)) -> Left . mayGroupersRadCom $ o
          Right (Moderate n) -> Right . mayGroupersRadPer $ n
          Right (Extreme (Polarized o _)) -> Right . mayGroupersRadPer $ o
    UC rnn _ ar -> Just (cy, ar, ei)
      where
        ei = case rnn of
          Left b -> Left $ mayGroupersRadCom b
          Right b -> Right $ mayGroupersRadPer b
    _ -> Nothing
  _ -> Nothing
