module PennyTest.Lincoln where

import qualified Test.Framework as T
import qualified PennyTest.Lincoln.Family as F

tests :: T.Test
tests = T.testGroup "Lincoln"
        [F.tests]
