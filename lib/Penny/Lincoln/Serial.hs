module Penny.Lincoln.Serial (
  Serial, forward, backward, serials,
  serialItems, serialChildrenInFamilies, serialParentsInFamilies,
  selectiveSerialParents,
  selectiveSerial) where

import Data.Foldable (foldl', toList)
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

-- | Applied to a list of items, return a list of Serials usable to
-- identify the list of items.
serials :: [a] -> [Serial]
serials as = zipWith Serial fs rs where
  len = length as
  fs = take len . iterate succ $ 0
  rs = take len . iterate pred $ (len - 1)


-- | Label a list of parents with serials, in order.
serialParentsInFamilies ::
  (Serial -> pOld -> pNew)
  -- ^ Function that takes the serial and the old parent, and returns
  -- a new parent.
  
  -> [F.Family pOld c]
  -> [F.Family pNew c]

serialParentsInFamilies f ls =
  let nf = 0
      nb = (length ls) - 1
      initState = (nf, nb)
      g a = do
        (fwd, bak) <- St.get
        let s = Serial fwd bak
        St.put (succ fwd, pred bak)
        return $ f s a
  in St.evalState (mapM (F.mapParentM g) ls) initState

-- | Label a list of children with serials, in order.
serialChildrenInFamilies ::
  (Serial -> cOld -> cNew)
  -- ^ Function that takes the serial and the old child, and returns a
  -- new child.
  
  -> [F.Family p cOld]
  -> [F.Family p cNew]
serialChildrenInFamilies f ls = 
  let nf = 0
      len = length . concatMap toList . map F.orphans $ ls
      nb = len - 1
      initState = (nf, nb)
  in St.evalState (mapM (serialChildrenInFamily f) ls) initState

serialChildrenInFamily ::
  (Serial -> cOld -> cNew)
  -> F.Family p cOld
  -> St.State (NextFwd, NextBack) (F.Family p cNew)
serialChildrenInFamily f = F.mapChildrenM (serialItemSt f)

type NextFwd = Int
type NextBack = Int

serialItemSt ::
  (Serial -> cOld -> cNew)
  -> cOld
  -> St.State (NextFwd, NextBack) cNew
serialItemSt f old = do
  (fwd, bak) <- St.get
  let s = Serial fwd bak
  St.put (succ fwd, pred bak)
  return (f s old)


selectiveSerialParents ::
  (Serial -> p -> p')
  -> (a -> Either (F.Family p c, F.Family p' c -> b) b)
  -> [a]
  -> [b]
selectiveSerialParents xformer selector as =
  let nf = 0
      fams = map fst . E.lefts . map selector $ as
      len = length fams
      initState = (0, len - 1)
      k = selectiveSerialParentsSt xformer selector
  in St.evalState (mapM k as) initState
      

selectiveSerialParentsSt ::
  (Serial -> p -> p')
  -> (a -> Either (F.Family p c, F.Family p' c -> b) b)
  -> a
  -> St.State (NextFwd, NextBack) b
selectiveSerialParentsSt xformer selector a =
  case selector a of
    Left ((F.Family p c1 c2 cs), toB) -> do
      (fwd, bak) <- St.get
      let s = Serial fwd bak
          fam = F.Family (xformer s p) c1 c2 cs
          b = toB fam
      St.put (succ fwd, pred bak)
      return b
    Right b -> return b

selectiveSerialChildren ::
  (Serial -> c -> c')
  -> (a -> Either (F.Family p c, F.Family p c' -> b) b)
  -> [a]
  -> [b]
selectiveSerialChildren xformer selector as =
  let nf = 0
      fams = map fst . E.lefts . map selector $ as
      len = length . concatMap toList . map F.orphans $ fams
      initState = (nf, len - 1)
      k = selectiveSerialChildrenSt xformer selector
  in St.evalState (mapM k as) initState


selectiveSerialChildrenSt ::
  (Serial -> c -> c')
  -> (a -> Either (F.Family p c, F.Family p c' -> b) b)
  -> a
  -> St.State (NextFwd, NextBack) b
selectiveSerialChildrenSt xformer selector a =
  case selector a of
    Left (fam, toB) -> do
      fam' <- serialChildrenInFamily xformer fam
      return $ toB fam'
    Right b -> return b

type PNextFwd = Int
type PNextBack = Int
type CNextFwd = Int
type CNextBack = Int
type State = (PNextFwd, PNextBack, CNextFwd, CNextBack)

selectiveSerial ::
  (Serial -> p -> p')
  -> (Serial -> c -> c')
  -> (a -> Either (F.Family p c, F.Family p' c' -> b) b)
  -> [a]
  -> [b]
selectiveSerial xformP xformC selector as =
  let pnf = 0
      plen = length as
      cnf = 0
      clen = length . concatMap toList . map F.orphans
             . map fst . E.lefts . map selector $ as
      initState = (pnf, plen - 1, cnf, clen - 1)
      k = selectiveSerialSt xformP xformC selector
  in St.evalState (mapM k as) initState

selectiveSerialF ::
  (Serial -> p -> p')
  -> (Serial -> c -> c')
  -> F.Family p c
  -> St.State State (F.Family p' c')
selectiveSerialF fp fc f =
  F.mapParentM (selectiveSerialPSt fp) f
  >>= F.mapChildrenM (selectiveSerialCSt fc)


selectiveSerialSt ::
  (Serial -> p -> p')
  -> (Serial -> c -> c')
  -> (a -> Either (F.Family p c, F.Family p' c' -> b) b)
  -> a
  -> St.State State b
selectiveSerialSt xformP xformC selector a =
  case selector a of
    Left (fam, f) -> do
      fam' <- selectiveSerialF xformP xformC fam
      return $ f fam'
    Right b -> return b

selectiveSerialCSt ::
  (Serial -> c -> c')
  -> c
  -> St.State State c'
selectiveSerialCSt f c = do
  (pnf, pnb, cnf, cnb) <- St.get
  let s = Serial cnf cnb
  St.put (pnf, pnb, succ cnf, pred cnb)
  return (f s c)

selectiveSerialPSt ::
  (Serial -> p -> p')
  -> p
  -> St.State State p'
selectiveSerialPSt f p = do
  (pnf, pnb, cnf, cnb) <- St.get
  let s = Serial pnf pnb
  St.put (succ pnf, pred pnb, cnf, cnb)
  return (f s p)

newtype Index = Index { unIndex :: Int } deriving (Eq, Ord, Show)
newtype Total = Total { unTotal :: Int } deriving (Eq, Ord, Show)

type NextA = Int
type NextB = Int

serialEithers ::
  Monad m
  => ([a] -> Serial -> a -> m c)
  -> ([b] -> Serial -> b -> m d)
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
            c <- lift $ fa allA (serial totA (Index nextA)) a
            St.put (succ nextA, nextB)
            return $ Left c
          Right b -> do
            d <- lift $ fb allB (serial totB (Index nextB)) b
            St.put (nextA, succ nextB)
            return $ Right d
  in St.evalStateT (mapM k ls) initState

serial :: Total -> Index -> Serial
serial (Total t) (Index i) = Serial i b where
  b = t - i - 1
