module Penny.Lincoln.Serial (
  Serial, forward, backward, GenSerial,
  incrementBack, get, makeSerials ) where

import Control.Applicative (Applicative, (<*>), pure)

data SerialSt = SerialSt
  { nextFwd :: Int
  , nextBack :: Int
  } deriving Show

data Serial = Serial
  { forward :: Int
  , backward :: Int
  } deriving Show

newtype GenSerial a = GenSerial (SerialSt -> (a, SerialSt))

instance Functor GenSerial where
  fmap f (GenSerial k) = GenSerial $ \s ->
    let (a', st') = k s
    in (f a', st')

instance Applicative GenSerial where
  pure a = GenSerial $ \s -> (a, s)
  (GenSerial f) <*> (GenSerial g) = GenSerial $ \s ->
    let (k, s') = f s
        (a, s'') = g s'
    in (k a, s'')

instance Monad GenSerial where
  return a = GenSerial $ \s -> (a, s)
  (GenSerial k) >>= f = GenSerial $ \s ->
    let (a, s') = k s
        GenSerial g = f a
    in g s'

incrementBack :: GenSerial ()
incrementBack = GenSerial $ \s ->
  let s' = SerialSt (nextFwd s) (nextBack s + 1)
  in ((), s')

get :: GenSerial Serial
get = GenSerial $ \s ->
  let s' = SerialSt (nextFwd s + 1) (nextBack s - 1)
  in (Serial (nextFwd s) (nextBack s), s')

makeSerials :: GenSerial a -> a
makeSerials (GenSerial k) =
  let (r, _) = k (SerialSt 0 0) in r
