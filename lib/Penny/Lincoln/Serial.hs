module Penny.Lincoln.Serial (
  Serial, forward, backward, serials,
  serialItems, serialItemsM,
  serialChildrenInFamily, serialEithers,
  NextFwd, NextBack, initNexts) where

import Data.Foldable (foldl', toList)
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import qualified Data.Either as E
import qualified Penny.Lincoln.Family as F
import qualified Control.Monad.Trans.State as St

-- | A type for serial numbers, used widely for different purposes
-- throughout Penny.

data Serial = Serial {
  -- | Numbered from first to last, beginning at zero.
  forward :: !Int
  
  -- | Numbered from last to first, ending at zero.
  , backward :: !Int

  } deriving (Eq, Show)
             

-- | Label a list of items with serials.
serialItems :: (Serial -> a -> b)
               -> [a]
               -> [b]
serialItems f as =
  let ss = serials as
  in zipWith f ss as

-- | Label a list of items with serials, in a monad.
serialItemsM ::
  Monad m
  => (Serial -> a -> m b)
  -> [a]
  -> m [b]
serialItemsM f as =
  let ss = serials as
  in zipWithM f ss as

-- | Applied to a list of items, return a list of Serials usable to
-- identify the list of items.
serials :: [a] -> [Serial]
serials as = zipWith Serial fs rs where
  len = length as
  fs = take len . iterate succ $ 0
  rs = take len . iterate pred $ (len - 1)


serialChildrenInFamily ::
  (Serial -> cOld -> cNew)
  -> F.Family p cOld
  -> St.State (NextFwd, NextBack) (F.Family p cNew)
serialChildrenInFamily f = F.mapChildrenM (serialItemSt f)

newtype NextFwd = NextFwd { unNextFwd :: Int } deriving Show
newtype NextBack = NextBack { unNextBack :: Int } deriving Show

serialItemSt ::
  (Serial -> cOld -> cNew)
  -> cOld
  -> St.State (NextFwd, NextBack) cNew
serialItemSt f old = do
  (NextFwd fwd, NextBack bak) <- St.get
  let s = Serial fwd bak
  St.put (NextFwd $ succ fwd, NextBack $ pred bak)
  return (f s old)

newtype Index = Index { unIndex :: Int } deriving (Eq, Ord, Show)
newtype Total = Total { unTotal :: Int } deriving (Eq, Ord, Show)

serialEithers ::
  Monad m
  => (Serial -> a -> m c)
  -> (Serial -> b -> m d)
  -> [Either a b]
  -> m [Either c d]
serialEithers fa fb ls =
  let allA = E.lefts ls
      allB = E.rights ls
      totA = Total . length $ allA
      totB = Total . length $ allB
      initState = (0 :: Int, 0 :: Int)
      k e = do
        (nextA, nextB) <- St.get
        case e of
          Left a -> do
            c <- lift $ fa (serial totA (Index nextA)) a
            St.put (succ nextA, nextB)
            return $ Left c
          Right b -> do
            d <- lift $ fb (serial totB (Index nextB)) b
            St.put (nextA, succ nextB)
            return $ Right d
  in St.evalStateT (mapM k ls) initState

serial :: Total -> Index -> Serial
serial (Total t) (Index i) = Serial i b where
  b = t - i - 1

initNexts :: Int -> (NextFwd, NextBack)
initNexts i = (NextFwd 0, NextBack $ i - 1)
