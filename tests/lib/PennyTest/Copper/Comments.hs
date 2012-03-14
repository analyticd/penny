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

-- | Generate a list of Char that is as long as the size
-- parameter. Will not generate the sequence "*/". Generates a few
-- extra * and /. List will have at least one character.
genRenCharList :: Gen [Char]
genRenCharList = sized $ \s ->
  genCharListRecurse [] (max 1 s)

genCharListRecurse :: [Char] -> Int -> Gen [Char]
genCharListRecurse str i =
  if i == 0
  then return str
  else let
    anyChar = frequency [(8, arbitrary), (1, return '*'),
                         (1, return '/')]
    noStar = frequency [ (8, suchThat arbitrary (/= '*'))
                       , (1, return '/')] 
    in case str of
      [] -> do
        c <- anyChar
        genCharListRecurse (c:[]) (pred i) 
      c:cs -> do
        c' <- case c of
          '/' -> noStar
          _ -> anyChar
        genCharListRecurse (c':c:cs) (pred i)

-- | Generate renderable text for multiline comment; include some *
-- and some /. Must have at least one character, or else it is not
-- renderable.
genRenMultilineText :: Gen X.Text
genRenMultilineText = X.pack <$> genRenCharList
  
-- | Generate renderable text for single-line comments. The text must
-- have at least one character, or else it is not renderable.
genRenSingleLineText :: Gen X.Text
genRenSingleLineText =
  X.pack <$> (listOf1 g) where
    g = suchThat arbitrary C.isSingleLineChar

genRenSingleLine :: Gen C.SingleLine
genRenSingleLine = C.SingleLine <$> genRenSingleLineText

genRenMultilineItem :: Gen C.MultilineItem
genRenMultilineItem = sized $ \s ->
  if s == 0
  then C.MultilineText <$> genRenMultilineText
  else resize ((max 5 (s - 1)))
       (oneof [ C.MultilineText <$> genRenMultilineText
              , C.Nested <$> genRenComment ])

genRenComment :: Gen C.Comment
genRenComment = oneof [ C.Single <$> genRenSingleLine
                      , C.Multi <$> genRenMultiline ]

genRenMultiline :: Gen C.Multiline
genRenMultiline = sized $ \s ->
  C.Multiline <$> vectorOf (min 4 s) genRenMultilineItem

newtype RenComment = RenComment C.Comment
                     deriving (Eq, Show)

instance Arbitrary RenComment where
  arbitrary = RenComment <$> genRenComment

-- | Generated comments are renderable
prop_genRenderable :: RenComment -> Bool
prop_genRenderable (RenComment c) =
  case C.render c of
    Nothing -> False
    Just _ -> True

test_genRenderable :: Test
test_genRenderable = testProperty s prop_genRenderable where
  s = "generated comments are renderable"

-- | Parsing a rendered comment yields the same thing.
prop_parseRendered :: RenComment -> Bool
prop_parseRendered (RenComment c) =
  case C.render c of
    Nothing -> False
    Just t -> case P.parse (C.comment <* P.eof) "" t of
      Left _ -> False
      Right c' -> c == c'

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing a rendered Comment yields the same thing"

tests :: Test
tests = testGroup "Comments"
        [ test_genRenderable
        , test_parseRendered ]
