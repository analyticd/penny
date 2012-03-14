module PennyTest.Copper.Comments where

import Control.Applicative ((<$>), (<*))
import qualified Data.Text as X
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (
  Arbitrary, arbitrary, suchThat, Gen,
  frequency, listOf, listOf1, sized, resize, oneof,
  vectorOf)

import qualified Penny.Copper.Comments as C

-- | Generate renderable comment characters.
genCommentChar :: Gen Char
genCommentChar = suchThat arbitrary C.isCommentChar

genRComment :: Gen C.Comment
genRComment = C.Comment . X.pack <$> listOf genCommentChar

newtype RComment = RComment C.Comment deriving (Show, Eq)
instance Arbitrary RComment where
  arbitrary = RComment <$> genRComment

-- | Parsing a rendered comment should give the same thing.
prop_parseRendered :: RComment -> Bool
prop_parseRendered (RComment c) = case C.render c of
  Nothing -> False
  Just t -> case P.parse (C.comment <* P.eof) "" t of
    Left _ -> False
    Right c' -> c == c'

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing rendered comment should yield the same thing"

tests :: Test
tests = testGroup "Comments"
        [ test_parseRendered ]
