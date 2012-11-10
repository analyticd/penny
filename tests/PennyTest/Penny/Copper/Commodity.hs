module PennyTest.Penny.Copper.Commodity where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck.Gen as G
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Gen (Gen)
import qualified PennyTest.Penny.Copper.Util as TU
import qualified Penny.Copper.Account as A
import qualified Penny.Lincoln as L
import qualified Text.Parsec as Ps

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Copper.Commodity"
  [
  ]

lvl1Char :: Gen Char
lvl1Char = G.suchThat (G.oneof [TU.unicodeAll, TU.asciiAll])
           (/= '"')

lvl1Cmdty :: Gen L.Commodity
lvl1Cmdty = (L.Commodity . X.pack) <$> G.listOf1 lvl1Char

lvl2FirstChar :: Gen Char
lvl2FirstChar = G.oneof [TU.unicodeAll, TU.letter, return '$']

lvl2OtherChars :: Gen Char
lvl2OtherChars = G.suchThat (G.oneof [TU.asciiAll, TU.unicodeAll])
                 (/= ' ')

lvl2Cmdty :: Gen L.Commodity
lvl2Cmdty = f <$> lvl2FirstChar <*> G.listOf lvl2OtherChars
  where
    f c cs = L.Commodity (X.pack (c:cs))

lvl3Char :: Gen Char
lvl3Char = G.oneof [TU.unicodeAll, TU.letter, return '$']

lvl3Cmdty :: Gen L.Commodity
lvl3Cmdty = (L.Commodity . X.pack) <$> G.listOf1 lvl3Char
