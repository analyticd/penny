module PennyTest.Copper.Memos where

import qualified PennyTest.Copper.Memos.Posting as P
import qualified PennyTest.Copper.Memos.Transaction as T
import Test.Framework (testGroup, Test)

tests :: Test
tests = testGroup "Memos"
        [ P.tests
        , T.tests ]
