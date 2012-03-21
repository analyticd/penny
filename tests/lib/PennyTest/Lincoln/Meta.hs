module PennyTest.Lincoln.Meta where

import qualified Penny.Lincoln.Meta as M

import Control.Applicative ((<$>), (<*>))
import Data.Text (pack)
import Test.QuickCheck (Arbitrary, arbitrary, elements, Gen)
import PennyTest.Lincoln.Bits (genMaybe)

genSide :: Gen M.Side
genSide = elements [ M.CommodityOnLeft, M.CommodityOnRight ]

genSpaceBetween :: Gen M.SpaceBetween
genSpaceBetween = elements [M.SpaceBetween, M.NoSpaceBetween]

genFormat :: Gen M.Format
genFormat = M.Format <$> genSide <*> genSpaceBetween

newtype Format = Format M.Format
                 deriving (Eq, Show)
instance Arbitrary Format where
  arbitrary = Format <$> genFormat

genLine :: Gen M.Line
genLine = M.Line <$> arbitrary

newtype AnyFilename = AnyFilename M.Filename
                      deriving (Eq, Show)
instance Arbitrary AnyFilename where
  arbitrary = AnyFilename <$> genFilename

genFilename :: Gen M.Filename
genFilename = M.Filename <$> (pack <$> arbitrary)

genColumn :: Gen M.Column
genColumn = M.Column <$> arbitrary

genPriceLine :: Gen M.PriceLine
genPriceLine = M.PriceLine <$> genLine

genPostingLine :: Gen M.PostingLine
genPostingLine = M.PostingLine <$> genLine

genTopMemoLine :: Gen M.TopMemoLine
genTopMemoLine = M.TopMemoLine <$> genLine

genTopLineLine :: Gen M.TopLineLine
genTopLineLine = M.TopLineLine <$> genLine

genPriceMeta :: Gen M.PriceMeta
genPriceMeta = M.PriceMeta
               <$> genMaybe genPriceLine
               <*> genMaybe genFormat

genPostingMeta :: Gen M.PostingMeta
genPostingMeta = M.PostingMeta
                 <$> genMaybe genPostingLine
                 <*> genMaybe genFormat

genTopLineMeta :: Gen M.TopLineMeta
genTopLineMeta = M.TopLineMeta
                 <$> genMaybe genTopMemoLine
                 <*> genMaybe genTopLineLine
                 <*> genMaybe genFilename
