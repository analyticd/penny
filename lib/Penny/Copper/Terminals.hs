module Penny.Copper.Terminals
  ( -- * Comments
    ivlCommentChar
  , CommentChar
  , commentChar
  , commentCharToChar
  , pCommentChar
  , rCommentChar

  -- * Non-escaped quoted characters
  , ivlNonEscapedChar
  , NonEscapedChar
  , nonEscapedChar
  , nonEscapedCharToChar
  , pNonEscapedChar
  , rNonEscapedChar

  -- * Unquoted string characters
  , ivlUSCharNonDigit
  , USCharNonDigit
  , usCharNonDigit
  , usCharNonDigitToChar
  , pUSCharNonDigit
  , rUSCharNonDigit

  -- * Typeclass
  , RangeTerm(..)
  ) where

import Penny.Copper.Intervals
import Control.Applicative
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Penny.Copper.Parser

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

