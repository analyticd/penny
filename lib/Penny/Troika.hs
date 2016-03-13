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

troiloadPole :: Troiload -> Maybe Pole
troiloadPole x = case x of
  QC q _ -> pole'RepAnyRadix q
  Q q -> pole'RepAnyRadix q
  SC qnz -> Just . nonZeroSign . _coefficient $ qnz
  S qnz -> Just . nonZeroSign . _coefficient $ qnz
  UC _ s _ -> Just s
  NC _ _ -> Nothing
  US _ s -> Just s
  UU _ -> Nothing
  C qnz -> Just . nonZeroSign . _coefficient $ qnz
  E qnz -> Just . nonZeroSign . _coefficient $ qnz

c'Decimal'Troiload :: Troiload -> Decimal
c'Decimal'Troiload x = case x of
  QC q _ -> c'Decimal'RepAnyRadix q
  Q q -> c'Decimal'RepAnyRadix q
  SC qnz -> fmap c'Integer'NonZero qnz
  S qnz -> fmap c'Integer'NonZero qnz
  UC rnn s _ ->
    fmap c'Integer'NonZero
    . c'DecNonZero'DecPositive s
    . c'DecPositive'BrimAnyRadix
    $ rnn
  NC nilAnyRadix _ ->
    fmap (const 0) . c'DecZero'NilAnyRadix $ nilAnyRadix
  US rnn s ->
    fmap c'Integer'NonZero
    . c'DecNonZero'DecPositive s
    . c'DecPositive'BrimAnyRadix
    $ rnn
  UU nil -> fmap (const 0) . c'DecZero'NilAnyRadix $ nil
  C qnz -> fmap c'Integer'NonZero qnz
  E qnz -> fmap c'Integer'NonZero qnz

type Troiquant = Either Troiload Decimal

data Troika = Troika
  { _commodity :: Commodity
  , _troiquant :: Either Troiload Decimal
  } deriving Show

makeLenses ''Troika

pole'Troika :: Troika -> Maybe Pole
pole'Troika (Troika _ ei) = case ei of
  Left tl -> pole'Decimal . c'Decimal'Troiload $ tl
  Right dec -> pole'Decimal dec

c'Amount'Troika :: Troika -> A.Amount
c'Amount'Troika (Troika cy ei) = A.Amount cy
  (either c'Decimal'Troiload id ei)

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
