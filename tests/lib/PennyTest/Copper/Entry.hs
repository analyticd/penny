module PennyTest.Copper.Entry where

import qualified Penny.Copper.Amount as A
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Meta as M
import qualified PennyTest.Copper.Amount as TA
-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Commodity (genRCmdty)
import PennyTest.Copper.Qty ()
import PennyTest.Copper.Util (wrapTextNonEmptyList)

import Control.Applicative ((<$>), (<*), (<*>))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, Gen)

