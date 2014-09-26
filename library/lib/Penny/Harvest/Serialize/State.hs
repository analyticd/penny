module Penny.Harvest.Serialize.State where

import Control.Applicative
import Control.Monad

newtype T a = T { runState :: (Int, Int) -> (a, (Int, Int)) }

instance Monad T where
  return a = T $ \pair -> (a, pair)
  (T l) >>= f = T $ \pair ->
    let (a, pair') = l pair
        T r = f a
    in r pair'

instance Applicative T where
  pure = return
  (<*>) = ap

instance Functor T where
  fmap = liftM

topLine :: T Int
topLine = T $ \(t, p) -> (t, (t, p))

topLineFwd :: T Int
topLineFwd = topLine <* incrTopLine

topLineRev :: T Int
topLineRev = decrTopLine *> topLine

posting :: T Int
posting = T $ \(t, p) -> (p, (t, p))

postingFwd :: T Int
postingFwd = posting <* incrPosting

postingRev :: T Int
postingRev = decrPosting *> posting

incrTopLine :: T ()
incrTopLine = T $ \(t, p) -> ((), (succ t, p))

incrPosting :: T ()
incrPosting = T $ \(t, p) -> ((), (t, succ p))

decrTopLine :: T ()
decrTopLine = T $ \(t, p) -> ((), (pred t, p))

decrPosting :: T ()
decrPosting = T $ \(t, p) -> ((), (t, pred p))
