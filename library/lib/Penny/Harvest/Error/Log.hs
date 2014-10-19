module Penny.Harvest.Error.Log where

import qualified Penny.Harvest.Error as Error
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Applicative
import Control.Monad
import Data.Monoid
import Prelude hiding (log)

data T a = T
  { errors :: Seq Error.T
  , payload :: a
  } deriving Show

toEither :: T a -> Either (Seq Error.T) a
toEither (T sq a)
  | Seq.null sq = Right a
  | otherwise = Left sq

log :: Error.T -> T ()
log e = T (Seq.singleton e) ()

instance Monad T where
  return = T Seq.empty
  (T errL payL) >>= f = T (errL <> errR) payR
    where
      T errR payR = f payL

instance Functor T where
  fmap = liftM

instance Applicative T where
  pure = return
  (<*>) = ap
