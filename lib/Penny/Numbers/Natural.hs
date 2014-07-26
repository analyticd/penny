module Penny.Numbers.Natural where

import Deka.Native.Abstract

data Pos = One | Succ Pos
  deriving (Eq, Ord, Show)

addPos :: Pos -> Pos -> Pos
addPos x One = Succ x
addPos x (Succ p) = addPos (Succ x) p

multPos :: Pos -> Pos -> Pos
multPos x One = x
multPos x (Succ p) = addPos x (multPos x p)

expPos :: Pos -> Pos -> Pos
expPos x One = x
expPos x (Succ p) = multPos x (expPos x p)

novemToPos :: Novem -> Pos
novemToPos n = case n of
  D1 -> One
  D2 -> Succ One
  D3 -> Succ (Succ One)
  D4 -> Succ (Succ (Succ One))
  D5 -> Succ (Succ (Succ (Succ One)))
  D6 -> Succ (Succ (Succ (Succ (Succ One))))
  D7 -> Succ (Succ (Succ (Succ (Succ (Succ One)))))
  D8 -> Succ (Succ (Succ (Succ (Succ (Succ (Succ One))))))
  D9 -> Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ One)))))))

data NonNeg = Zero | NonZero Pos
  deriving (Eq, Ord, Show)

nextNonNeg :: NonNeg -> NonNeg
nextNonNeg z = case z of
  Zero -> NonZero One
  NonZero p -> NonZero $ Succ p

addNonNeg :: NonNeg -> NonNeg -> NonNeg
addNonNeg x Zero = x
addNonNeg x (NonZero p) = case x of
  Zero -> NonZero p
  NonZero l -> NonZero $ addPos l p

multNonNeg :: NonNeg -> NonNeg -> NonNeg
multNonNeg _ Zero = Zero
multNonNeg x (NonZero p) = case x of
  Zero -> Zero
  NonZero l -> NonZero $ multPos l p

expNonNeg :: NonNeg -> NonNeg -> NonNeg
expNonNeg _ Zero = NonZero One
expNonNeg x (NonZero p) = case x of
  Zero -> Zero
  NonZero l -> NonZero $ expPos l p

decemToNonNeg :: Decem -> NonNeg
decemToNonNeg d = case d of
  D0 -> Zero
  Nonem n -> NonZero $ novemToPos n

tenNonNeg :: NonNeg
tenNonNeg = nextNonNeg . decemToNonNeg . Nonem $ D9

tenPos :: Pos
tenPos = Succ . novemToPos $ D9
