module Main where

import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import Test.QuickCheck
import Test.QuickCheck.Gen
import Penny.Copper.DateTime
import Penny.Copper.Qty
import Penny.Copper.Item
import PennyTest.Copper.Item
import qualified Penny.Copper.Item as I
import Penny.Lincoln.Bits
import System.Environment (getArgs)
import System.Random (mkStdGen)

dtz :: DefaultTimeZone
dtz = case minsToOffset ((-4) * 60) of
  Nothing -> error "could not make time zone"
  Just o -> DefaultTimeZone o

groupSpec :: (GroupingSpec, GroupingSpec)
groupSpec = (GroupAll, GroupAll)

radGroup :: RadGroup
radGroup = periodComma

genItems :: Int -> Gen [Item]
genItems i = vectorOf i (genRItem dtz)

showAll :: [Item] -> X.Text
showAll is = case mapM (I.render dtz groupSpec radGroup) is of
  Nothing -> error "render failed"
  Just xs -> X.concat xs

main :: IO ()
main = do
  as <- getArgs
  let (num:seed:size:_) = fmap read as
      items = unGen (genItems num) (mkStdGen seed) size
      x = showAll items
  TIO.putStr x
