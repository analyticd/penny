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
  , ivlUSCharOpt
  , USCharOpt
  , usCharOpt
  , usCharOptToChar
  , pUSCharOpt
  , rUSCharOpt

  , ivlUSCharReq
  , USCharReq
  , usCharReq
  , usCharReqToChar
  , pUSCharReq
  , rUSCharReq

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
import Penny.Copper.Parser
import Penny.Copper.Classes

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

instance Parseable CommentChar where parser = pCommentChar
instance Renderable CommentChar where render = rCommentChar

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

instance Parseable NonEscapedChar where parser = pNonEscapedChar
instance Renderable NonEscapedChar where render = rNonEscapedChar

ivlUSCharOpt :: Intervals Char
ivlUSCharOpt
  = Intervals [range minBound maxBound]
  . map singleton
  $ [ ' ', '\\', '\n', '\t', '{', '}', '[', ']', '\'', '"',
      '#', '@', '`' ]

newtype USCharOpt = USCharOpt
  { usCharOptToChar :: Char }
  deriving (Eq, Ord, Show)

usCharOpt :: Char -> Maybe USCharOpt
usCharOpt c
  | c `inIntervals` ivlUSCharOpt = Just (USCharOpt c)
  | otherwise = Nothing

pUSCharOpt :: Parser USCharOpt
pUSCharOpt = USCharOpt
  <$> rangeToParser ivlUSCharOpt

rUSCharOpt :: USCharOpt -> ShowS
rUSCharOpt (USCharOpt c) = (c:)

instance Parseable USCharOpt where parser = pUSCharOpt
instance Renderable USCharOpt where render = rUSCharOpt

ivlUSCharReq :: Intervals Char
ivlUSCharReq = addExcluded (range '0' '9') ivlUSCharOpt

newtype USCharReq = USCharReq { usCharReqToChar :: Char }
  deriving (Eq, Ord, Show)

usCharReq :: Char -> Maybe USCharReq
usCharReq c
  | c `inIntervals` ivlUSCharReq = Just . USCharReq $ c
  | otherwise = Nothing

pUSCharReq :: Parser USCharReq
pUSCharReq = USCharReq <$> rangeToParser ivlUSCharReq

rUSCharReq :: USCharReq -> ShowS
rUSCharReq (USCharReq c) = (c:)

instance Parseable USCharReq where parser = pUSCharReq
instance Renderable USCharReq where render = rUSCharReq

newtype UnquotedCommodityFirstChar
  = UnquotedCommodityFirstChar { unquotedCommodityFirstCharToChar :: Char }
  deriving (Eq, Ord, Show)

ivlUnquotedCommodityFirstChar :: Intervals Char
ivlUnquotedCommodityFirstChar = ivlUSCharOpt `remove`
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

instance Parseable UnquotedCommodityFirstChar where
  parser = pUnquotedCommodityFirstChar
instance Renderable UnquotedCommodityFirstChar where
  render = rUnquotedCommodityFirstChar

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

instance RangeTerm USCharOpt where
  termIntervals = const ivlUSCharOpt
  termFromChar = usCharOpt
  termToChar = usCharOptToChar
  pRangeTerm = pUSCharOpt

instance RangeTerm UnquotedCommodityFirstChar where
  termIntervals = const ivlUnquotedCommodityFirstChar
  termFromChar = unquotedCommodityFirstChar
  termToChar = unquotedCommodityFirstCharToChar
  pRangeTerm = pUnquotedCommodityFirstChar

