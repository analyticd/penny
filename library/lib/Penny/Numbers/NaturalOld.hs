module Penny.Numbers.NaturalOld where

import Deka.Native.Abstract
import qualified Data.Foldable as F

data Pos = One | Succ Pos
  deriving (Eq, Ord, Show)

addPos :: Pos -> Pos -> Pos
addPos x One = Succ x
addPos x (Succ p) = addPos (Succ x) p

multPos :: Pos -> Pos -> Pos
multPos x One = x
multPos x (Succ p) = addPos x (multPos x p)

subtPos :: Pos -> Pos -> Maybe NonNeg
subtPos x y = case x of
  One -> case y of
    One -> Just Zero
    _ -> Nothing
  Succ l -> case y of
    One -> Just (NonZero l)
    Succ r -> subtPos l r

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

posToNovem :: Pos -> Maybe Novem
posToNovem p
  | p == novemToPos D1 = Just D1
  | p == novemToPos D2 = Just D2
  | p == novemToPos D3 = Just D3
  | p == novemToPos D4 = Just D4
  | p == novemToPos D5 = Just D5
  | p == novemToPos D6 = Just D6
  | p == novemToPos D7 = Just D7
  | p == novemToPos D8 = Just D8
  | p == novemToPos D9 = Just D9
  | otherwise = Nothing

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


subtPosFromNonNeg :: NonNeg -> Pos -> Maybe NonNeg
subtPosFromNonNeg x y = case x of
  Zero -> Nothing
  NonZero l -> subtPos l y

multNonNeg :: NonNeg -> NonNeg -> NonNeg
multNonNeg _ Zero = Zero
multNonNeg x (NonZero p) = case x of
  Zero -> Zero
  NonZero l -> NonZero $ multPos l p

divNonNegByPos :: NonNeg -> Pos -> (NonNeg, NonNeg)
divNonNegByPos x y = go Zero x
  where
    go soFar l = case subtPosFromNonNeg l y of
      Nothing -> (soFar, l)
      Just l' -> go soFar' l'
        where
          soFar' = case soFar of
            Zero -> NonZero One
            NonZero nz -> NonZero (Succ nz)

expNonNeg :: NonNeg -> NonNeg -> NonNeg
expNonNeg _ Zero = NonZero One
expNonNeg x (NonZero p) = case x of
  Zero -> Zero
  NonZero l -> NonZero $ expPos l p

decemToNonNeg :: Decem -> NonNeg
decemToNonNeg d = case d of
  D0 -> Zero
  Nonem n -> NonZero $ novemToPos n

nonNegToDecem :: NonNeg -> Maybe Decem
nonNegToDecem n = case n of
  Zero -> Just D0
  NonZero p -> fmap Nonem $ posToNovem p

tenNonNeg :: NonNeg
tenNonNeg = nextNonNeg . decemToNonNeg . Nonem $ D9

tenPos :: Pos
tenPos = Succ . novemToPos $ D9

length :: F.Foldable f => f a -> NonNeg
length = F.foldl' (\a _ -> nextNonNeg a) Zero
