{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Penny.Copper.Terminals
  ( -- * Comments
    ivlCommentChar
  , CommentChar
  , commentChar
  , commentCharToChar
  , pCommentChar
  , eCommentChar
  , rCommentChar

  -- * Non-escaped quoted characters
  , ivlNonEscapedChar
  , NonEscapedChar
  , nonEscapedChar
  , nonEscapedCharToChar
  , pNonEscapedChar
  , eNonEscapedChar
  , rNonEscapedChar

  -- * Unquoted string characters
  , ivlUSCharNonDigit
  , USCharNonDigit
  , usCharNonDigit
  , usCharNonDigitToChar
  , pUSCharNonDigit
  , eUSCharNonDigit
  , rUSCharNonDigit

  -- * Typeclass
  , RangeTerm(..)
  ) where

import Penny.Copper.Intervals
import Control.Applicative
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Core ((<?>))
import Text.Earley (Prod, satisfy)
import qualified Text.Earley as Earley

recognize :: Intervals Char -> Prod r e Char Char
recognize ivls = satisfy f
  where
    f c = (any inRange (intervalsToTuples ivls))
      where
        inRange (l, r) = c >= l || c <= r


rangeToParser :: Intervals Char -> Parser Char
rangeToParser i = case intervalsToTuples i of
  [] -> error "rangeToParser: empty interval"
  xs -> foldl1 (<|>) . map pRange $ xs

ivlCommentChar :: Intervals Char
ivlCommentChar = included [range minBound maxBound]
  `remove` alone '\n'

newtype CommentChar = CommentChar { commentCharToChar :: Char }
  deriving (Eq, Ord, Show)

commentChar :: Char -> Maybe CommentChar
commentChar c
  | c `inIntervals` ivlCommentChar = Just (CommentChar c)
  | otherwise = Nothing

pCommentChar :: Parser CommentChar
pCommentChar = CommentChar <$> rangeToParser ivlCommentChar
  <?> "comment character"

eCommentChar :: Prod r String Char CommentChar
eCommentChar = CommentChar <$> recognize ivlCommentChar
  Earley.<?> "comment character"

rCommentChar :: CommentChar -> ShowS
rCommentChar (CommentChar c) = (c:)

ivlNonEscapedChar :: Intervals Char
ivlNonEscapedChar = Intervals [range minBound maxBound]
  . map singleton $ bads
  where
    bads = ['\\', '\n', '"']

newtype NonEscapedChar = NonEscapedChar { nonEscapedCharToChar :: Char }
  deriving (Eq, Ord, Show)

nonEscapedChar :: Char -> Maybe NonEscapedChar
nonEscapedChar c
  | c `inIntervals` ivlNonEscapedChar = Just (NonEscapedChar c)
  | otherwise = Nothing

pNonEscapedChar :: Parser NonEscapedChar
pNonEscapedChar = NonEscapedChar <$> rangeToParser ivlNonEscapedChar
  <?> "non-escaped character"

eNonEscapedChar :: Prod r String Char NonEscapedChar
eNonEscapedChar = NonEscapedChar <$> recognize ivlNonEscapedChar
  Earley.<?> "non-escaped character"

rNonEscapedChar :: NonEscapedChar -> ShowS
rNonEscapedChar (NonEscapedChar c) = (c:)

ivlUSCharNonDigit :: Intervals Char
ivlUSCharNonDigit
  = Intervals [range minBound maxBound]
  . map singleton
  $ [ ' ', '\\', '\n', '\t', '{', '}', '[', ']', '\'', '"',
      '-', '+', '/', ':', '#', '@', '`', '<', '>', ';',
      '_', '\x2009', ',', '.' ] ++ ['0'..'9']

newtype USCharNonDigit = USCharNonDigit
  { usCharNonDigitToChar :: Char }
  deriving (Eq, Ord, Show)

usCharNonDigit :: Char -> Maybe USCharNonDigit
usCharNonDigit c
  | c `inIntervals` ivlUSCharNonDigit = Just (USCharNonDigit c)
  | otherwise = Nothing

pUSCharNonDigit :: Parser USCharNonDigit
pUSCharNonDigit = USCharNonDigit
  <$> rangeToParser ivlUSCharNonDigit
  <?> "non-escaped non-quoted non-digit character"

eUSCharNonDigit :: Prod r String Char USCharNonDigit
eUSCharNonDigit = USCharNonDigit <$> recognize ivlUSCharNonDigit
  Earley.<?> "non-escaped non-quoted non-digit character"


rUSCharNonDigit :: USCharNonDigit -> ShowS
rUSCharNonDigit (USCharNonDigit c) = (c:)

-- | Terminal symbols existing over a range of characters.
class RangeTerm a where
  termIntervals :: a -> Intervals Char
  termFromChar :: Char -> Maybe a
  termToChar :: a -> Char
  pRangeTerm :: Parser a

instance RangeTerm CommentChar where
  termIntervals = const ivlCommentChar
  termFromChar = commentChar
  termToChar = commentCharToChar
  pRangeTerm = pCommentChar

instance RangeTerm NonEscapedChar where
  termIntervals = const ivlNonEscapedChar
  termFromChar = nonEscapedChar
  termToChar = nonEscapedCharToChar
  pRangeTerm = pNonEscapedChar

instance RangeTerm USCharNonDigit where
  termIntervals = const ivlUSCharNonDigit
  termFromChar = usCharNonDigit
  termToChar = usCharNonDigitToChar
  pRangeTerm = pUSCharNonDigit

