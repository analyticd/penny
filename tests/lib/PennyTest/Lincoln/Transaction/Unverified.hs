module PennyTest.Lincoln.Transaction.Unverified where

import Control.Applicative ((<$>), (<*>))
import qualified PennyTest.Lincoln.Bits as TB
import qualified Penny.Lincoln.Transaction.Unverified as U
import Test.QuickCheck (Gen)

genUniTopLine :: Gen U.TopLine
genUniTopLine =
  U.TopLine
  <$> TB.genDateTime
  <*> TB.genMaybe TB.genUniFlag
  <*> TB.genMaybe TB.genUniNumber
  <*> TB.genMaybe TB.genUniPayee
  <*> TB.genUniMemo

genUniPosting :: Gen U.Posting
genUniPosting =
  U.Posting
  <$> TB.genMaybe TB.genUniPayee
  <*> TB.genMaybe TB.genUniNumber
  <*> TB.genMaybe TB.genUniFlag
  <*> TB.genUniAccount
  <*> TB.genUniTags
  <*> TB.genMaybe TB.genUniEntry
  <*> TB.genUniMemo
