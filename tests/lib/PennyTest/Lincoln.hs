module PennyTest.Lincoln where

import qualified Test.Framework as T
import qualified PennyTest.Lincoln.Balance as B
import qualified PennyTest.Lincoln.Family as F
import qualified PennyTest.Lincoln.Transaction as Transaction

tests :: T.Test
tests = T.testGroup "Lincoln"
        [ B.tests
        , F.tests
        , Transaction.tests ]
