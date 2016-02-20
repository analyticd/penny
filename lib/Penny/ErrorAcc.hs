{-# LANGUAGE TemplateHaskell #-}
module Penny.ErrorAcc where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Control.Lens as Lens

-- | An accumulating error type.

data ErrorAcc e a
  = Failures (Seq e)
  | Successes a
  deriving (Eq, Ord, Show)

instance Functor (ErrorAcc e) where
  fmap f x = case x of
    Failures a -> Failures a
    Successes a -> Successes (f a)

Lens.makePrisms ''ErrorAcc

flunk :: e -> ErrorAcc e a
flunk e = Failures (Seq.singleton e)

pass :: a -> ErrorAcc e a
pass = Successes

eitherToAcc :: Either e a -> ErrorAcc e a
eitherToAcc = either flunk pass

instance Monoid a => Monoid (ErrorAcc e a) where
  mempty = Successes mempty
  mappend a b = case (a, b) of
    (Failures l, Failures r) -> Failures (l `mappend` r)
    (Successes l, Successes r) -> Successes (l `mappend` r)
    (Failures l, Successes _) -> Failures l
    (Successes _, Failures r) -> Failures r
