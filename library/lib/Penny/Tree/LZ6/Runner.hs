module Penny.Tree.LZ6.Runner where

import qualified Penny.Tree.LZ6.Collector as Collector
import qualified Penny.Tree.LZ6.Zero as Zero
import Prelude hiding (foldr)
import qualified Penny.Tree.LZ6 as LZ6
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Data.Sequence ((<|))
import qualified Data.Sequence as S

foldr
  :: (Zeroes.T -> a -> b -> b)
  -> (Zero.T a -> b)
  -> LZ6.T a
  -> b
foldr f fz lz6 = case lz6 of
  LZ6.Novem nd sq -> fz (Zero.Novem nd sq)
  LZ6.Zero zs my -> case my of
    Nothing -> fz (Zero.ZeroOnly zs)
    Just ei -> case ei of
      Left (nd, sq) -> fz (Zero.ZeroNovSeq zs nd sq)
      Right (a, lz6') -> f zs a (foldr f fz lz6')

runFold :: LZ6.T a -> Collector.T a
runFold = foldr ff fz
  where
    ff zs a (Collector.T sq z) = Collector.T ((zs, a) <| sq) z
    fz = Collector.T S.empty
