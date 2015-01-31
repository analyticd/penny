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
  , ivlUnquotedStringChar
  , UnquotedStringChar
  , unquotedStringChar
  , unquotedStringCharToChar
  , pUnquotedStringChar
  , rUnquotedStringChar

  -- * Unquoted commodity first character
  , ivlUnquotedCommodityFirstChar
  , UnquotedCommodityFirstChar
  , unquotedCommodityFirstChar
  , unquotedCommodityFirstCharToChar
  , pUnquotedCommodityFirstChar
  , rUnquotedCommodityFirstChar

  -- * Typeclass
  , RangeTerm(..)
  ) where

import Penny.Copper.Intervals
import Control.Applicative
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Penny.Copper.Classes
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

ivlUnquotedStringChar :: Intervals Char
ivlUnquotedStringChar
  = Intervals [range minBound maxBound]
  . map singleton
  $ [ ' ', '\\', '\n', '\t', '{', '}', '[', ']', '\'', '"',
      '#', '@', '`']

newtype UnquotedStringChar = UnquotedStringChar
  { unquotedStringCharToChar :: Char }
  deriving (Eq, Ord, Show)

unquotedStringChar :: Char -> Maybe UnquotedStringChar
unquotedStringChar c
  | c `inIntervals` ivlUnquotedStringChar = Just (UnquotedStringChar c)
  | otherwise = Nothing

pUnquotedStringChar :: Parser UnquotedStringChar
pUnquotedStringChar = UnquotedStringChar
  <$> rangeToParser ivlUnquotedStringChar

rUnquotedStringChar :: UnquotedStringChar -> ShowS
rUnquotedStringChar (UnquotedStringChar c) = (c:)

newtype UnquotedCommodityFirstChar
  = UnquotedCommodityFirstChar { unquotedCommodityFirstCharToChar :: Char }
  deriving (Eq, Ord, Show)

ivlUnquotedCommodityFirstChar :: Intervals Char
ivlUnquotedCommodityFirstChar = ivlUnquotedStringChar `remove`
  included [range '0' '9']

unquotedCommodityFirstChar :: Char -> Maybe UnquotedCommodityFirstChar
unquotedCommodityFirstChar c
  | c `inIntervals` ivlUnquotedCommodityFirstChar =
      Just . UnquotedCommodityFirstChar $ c
  | otherwise = Nothing

pUnquotedCommodityFirstChar :: Parser UnquotedCommodityFirstChar
pUnquotedCommodityFirstChar = UnquotedCommodityFirstChar <$>
  rangeToParser ivlUnquotedCommodityFirstChar

rUnquotedCommodityFirstChar :: UnquotedCommodityFirstChar -> ShowS
rUnquotedCommodityFirstChar (UnquotedCommodityFirstChar c) = (c:)

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

instance RangeTerm UnquotedStringChar where
  termIntervals = const ivlUnquotedStringChar
  termFromChar = unquotedStringChar
  termToChar = unquotedStringCharToChar
  pRangeTerm = pUnquotedStringChar

instance RangeTerm UnquotedCommodityFirstChar where
  termIntervals = const ivlUnquotedCommodityFirstChar
  termFromChar = unquotedCommodityFirstChar
  termToChar = unquotedCommodityFirstCharToChar
  pRangeTerm = pUnquotedCommodityFirstChar

