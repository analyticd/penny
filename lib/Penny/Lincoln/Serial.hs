{-# LANGUAGE DeriveGeneric #-}

module Penny.Lincoln.Serial (
  Serial, forward, backward, GenSerial,
  incrementBack, getSerial, makeSerials, serialItems,
  nSerials ) where

import Control.Applicative (Applicative, (<*>), pure, (*>))
import Control.Monad (ap)
import GHC.Generics (Generic)
import Data.Binary (Binary)

data SerialSt = SerialSt
  { nextFwd :: Int
  , nextBack :: Int
  } deriving Show

data Serial = Serial
  { forward :: Int
  , backward :: Int
  } deriving (Eq, Show, Ord, Generic)

instance Binary Serial

newtype GenSerial a = GenSerial (SerialSt -> (a, SerialSt))

instance Functor GenSerial where
  fmap f (GenSerial k) = GenSerial $ \s ->
    let (a', st') = k s
    in (f a', st')

instance Applicative GenSerial where
  pure = return
  (<*>) = ap

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

getSerial :: GenSerial Serial
getSerial = GenSerial $ \s ->
  let s' = SerialSt (nextFwd s + 1) (nextBack s - 1)
  in (Serial (nextFwd s) (nextBack s), s')

makeSerials :: GenSerial a -> a
makeSerials (GenSerial k) =
  let (r, _) = k (SerialSt 0 0) in r

serialItems :: (Serial -> a -> b) -> [a] -> [b]
serialItems f as = zipWith f (nSerials (length as)) as

nSerials :: Int -> [Serial]
nSerials n =
  makeSerials $
  (sequence . replicate n $ incrementBack)
  *> (sequence . replicate n $ getSerial)
