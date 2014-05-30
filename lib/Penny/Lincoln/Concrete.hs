module Penny.Lincoln.Concrete where

import qualified Penny.Lincoln.Abstract as A
import qualified Deka.Dec as D
import Data.Monoid ((<>))
import Prelude hiding (negate)

newtype Concrete = Concrete { unConcrete :: D.Dec }
  deriving Show

class Finite a where
  concrete :: a -> Concrete

class MaybeFinite a where
  maybeConcrete :: a -> Maybe Concrete

instance MaybeFinite D.Dec where
  maybeConcrete a
    | D.isFinite a = Just (Concrete a)
    | otherwise = Nothing

compute :: D.Ctx a -> a
compute c
  | fl == D.emptyFlags = r
  | otherwise = error
        "Penny.Lincoln.Concrete: computation out of range"
  where
    (r, fl) = D.runCtxStatus c

add :: Concrete -> Concrete -> Concrete
add (Concrete x) (Concrete y) = Concrete . compute $
  D.add x y

subt :: Concrete -> Concrete -> Concrete
subt (Concrete x) (Concrete y) = Concrete . compute $
  D.subtract x y

mult :: Concrete -> Concrete -> Concrete
mult (Concrete x) (Concrete y) = Concrete . compute $
  D.multiply x y

negate :: Concrete -> Concrete
negate (Concrete x) = Concrete . compute $ D.minus x

instance Finite (A.Whole a) where
  concrete (A.Whole c) = Concrete . compute $ D.fromByteString bs
    where
      bs = undefined

