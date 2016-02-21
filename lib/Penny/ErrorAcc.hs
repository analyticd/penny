{-# LANGUAGE TemplateHaskell #-}
module Penny.ErrorAcc where

import Data.Semigroup (Semigroup((<>)))
import qualified Data.Sequence as Seq
import qualified Control.Lens as Lens

import Penny.NonEmpty

-- | An accumulating error type.

data ErrorAcc e a
  = Failures (NonEmpty e)
  | Successes a
  deriving (Eq, Ord, Show)

instance Functor (ErrorAcc e) where
  fmap f x = case x of
    Failures a -> Failures a
    Successes a -> Successes (f a)

Lens.makePrisms ''ErrorAcc

flunk :: e -> ErrorAcc e a
flunk e = Failures (NonEmpty e Seq.empty)

pass :: a -> ErrorAcc e a
pass = Successes

eitherToAcc :: Either e a -> ErrorAcc e a
eitherToAcc = either flunk pass

accToEither :: ErrorAcc e a  -> Either (NonEmpty e) a
accToEither (Failures e) = Left e
accToEither (Successes a) = Right a

instance Monoid a => Monoid (ErrorAcc e a) where
  mempty = Successes mempty
  mappend a b = case (a, b) of
    (Failures l, Failures r) -> Failures (l <> r)
    (Successes l, Successes r) -> Successes (l `mappend` r)
    (Failures l, Successes _) -> Failures l
    (Successes _, Failures r) -> Failures r
