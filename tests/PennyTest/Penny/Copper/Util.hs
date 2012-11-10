module PennyTest.Penny.Copper.Util where

import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Gen (Gen)
import qualified Penny.Copper.Util as U

allChars :: Gen Char
allChars = G.choose (minBound, maxBound)

unicodeAll :: Gen Char
unicodeAll = G.suchThat allChars U.unicodeAll

letter :: Gen Char
letter = G.oneof [G.choose ('A', 'Z'), G.choose ('a', 'z')]

digit :: Gen Char
digit = G.choose ('0', '9')

asciiAll :: Gen Char
asciiAll = G.choose (' ', pred '\x7f')

