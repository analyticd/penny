module PennyTest.Copper where

import qualified Test.Framework as TF

import qualified PennyTest.Copper.Account as A
import qualified PennyTest.Copper.Amount as Amount
import qualified PennyTest.Copper.Comments as Comments
import qualified PennyTest.Copper.Commodity as C
import qualified PennyTest.Copper.DateTime as DT
import qualified PennyTest.Copper.Entry as Entry
import qualified PennyTest.Copper.Flag as Flag
import qualified PennyTest.Copper.Memos as Memos
import qualified PennyTest.Copper.Number as Number
import qualified PennyTest.Copper.Payees as Payees
import qualified PennyTest.Copper.Posting as Posting
import qualified PennyTest.Copper.Price as Price
import qualified PennyTest.Copper.Qty as Q
import qualified PennyTest.Copper.Tags as Tags
import qualified PennyTest.Copper.TopLine as TopLine

tests :: TF.Test
tests = TF.testGroup "Copper"
        [
          A.tests
        , Amount.tests
        , Comments.tests
        , C.tests
        , DT.tests
        , Entry.tests
        , Flag.tests
        , Memos.tests
        , Number.tests
        , Payees.tests
        , Posting.tests
        , Price.tests
        , Q.tests
        , Tags.tests
        , TopLine.tests
        ]
