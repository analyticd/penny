module PennyTest.Copper.Payees where

import qualified Penny.Copper.Payees as P
import qualified Penny.Lincoln.Bits as B

-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Qty ()
import PennyTest.Copper.Util (genTextNonEmpty)

import Control.Applicative ((<$>), (<*))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, Gen, oneof)

-- | Generates payees that do not need to be quoted.
genNoQuotePayee :: Gen B.Payee
genNoQuotePayee = B.Payee <$> genTextNonEmpty f r where
  f = suchThat arbitrary P.unquotedFirstChar
  r = suchThat arbitrary P.unquotedRestChars

-- | Generates payees that are renderable but might need quoting.
genNeedsQuotePayee :: Gen B.Payee
genNeedsQuotePayee = B.Payee <$> genTextNonEmpty c c where
  c = suchThat arbitrary P.quotedChar

newtype NoQuotePayee = NoQuotePayee { unNoQuotePayee :: B.Payee }
                       deriving (Eq, Show)

instance Arbitrary NoQuotePayee where
  arbitrary = NoQuotePayee <$> genNoQuotePayee

newtype NeedsQuotePayee =
  NeedsQuotePayee { unNeedsQuotePayee :: B.Payee }
  deriving (Eq, Show)

instance Arbitrary NeedsQuotePayee where
  arbitrary = NeedsQuotePayee <$> genNeedsQuotePayee

-- | Generates a renderable payee. These are evenly distributed
-- between payees that need quoting and those that do not need
-- quoting.
genRPayee :: Gen B.Payee
genRPayee = oneof [genNoQuotePayee, genNeedsQuotePayee]

newtype RPayee = RPayee { unRPayee :: B.Payee }
                 deriving (Eq, Show)

instance Arbitrary RPayee where
  arbitrary = RPayee <$> genRPayee

-- | Parsing a renderable payee should give the same thing.
prop_parsePayee :: RPayee -> Bool
prop_parsePayee (RPayee p) = case P.smartRender p of
  Nothing -> False
  Just t -> case P.parse (P.payee <* P.eof) "" t of
    Left _ -> False
    Right p' -> p == p'

test_parsePayee :: Test
test_parsePayee = testProperty s prop_parsePayee where
  s = "Parsing renderable Payee should give the same thing"

tests :: Test
tests = testGroup "Payees"
        [ test_parsePayee ]
