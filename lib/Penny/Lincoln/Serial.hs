module Penny.Lincoln.Serial (
  Serial, forward, backward, serials,
  serialItems, serialChildren, serialParents) where

import Data.Foldable (foldl', toList)
import qualified Penny.Lincoln.Family as F
import qualified Control.Monad.Trans.State as St

-- | A type for serial numbers, used widely for different purposes
-- throughout Penny.

data Serial = Serial {
  -- | Numbered from first to last, beginning at zero.
  forward :: !Int
  
  -- | Numbered from last to first, ending at zero.
  , backward :: !Int
  } deriving Show
             

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
serialParents ::
  (Serial -> pOld -> pNew)
  -- ^ Function that takes the serial and the old parent, and returns
  -- a new parent.
  
  -> [F.Family pOld c]
  -> [F.Family pNew c]

serialParents f ls =
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
serialChildren ::
  (Serial -> cOld -> cNew)
  -- ^ Function that takes the serial and the old child, and returns a
  -- new child.
  
  -> [F.Family p cOld]
  -> [F.Family p cNew]
serialChildren f ls = 
  let nf = 0
      len = length . concatMap toList . map F.orphans $ ls
      nb = len - 1
      initState = (nf, nb)
  in St.evalState (mapM (serialFam f) ls) initState

serialFam ::
  (Serial -> cOld -> cNew)
  -> F.Family p cOld
  -> St.State (NextFwd, NextBack) (F.Family p cNew)
serialFam f = F.mapChildrenM (serialItem f)

type NextFwd = Int
type NextBack = Int

serialItem ::
  (Serial -> cOld -> cNew)
  -> cOld
  -> St.State (NextFwd, NextBack) cNew
serialItem f old = do
  (fwd, bak) <- St.get
  let s = Serial fwd bak
  St.put (succ fwd, pred bak)
  return (f s old)
