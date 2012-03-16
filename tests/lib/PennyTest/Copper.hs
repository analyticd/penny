module PennyTest.Copper where

import qualified Test.Framework as TF

import qualified PennyTest.Copper.Account as A
import qualified PennyTest.Copper.Amount as Amount
import qualified PennyTest.Copper.Comments as Comments
import qualified PennyTest.Copper.Commodity as C
import qualified PennyTest.Copper.DateTime as DT
import qualified PennyTest.Copper.Entry as Entry
import qualified PennyTest.Copper.Qty as Q

tests :: TF.Test
tests = TF.testGroup "Copper"
        [
          A.tests
        , Amount.tests
        , Comments.tests
        , C.tests
        , DT.tests
        , Entry.tests
        , Q.tests
        ]
