{-# LANGUAGE TemplateHaskell #-}

-- | The functions and values in "Penny.Copper.Copperize" produce
-- output that is correct, but ugly.  This module formats values
-- nicely.  Useful for formatting freshly copperized values, and also
-- for cleaning up ledger files that were written by humans.
module Penny.Copper.Formatter where

import Penny.Copper.Copperize
import Penny.Copper.Optics
import Penny.Copper.Types
import Penny.SeqUtil (Groups, sortNonEmptySeq)
import qualified Penny.SeqUtil as SeqUtil

import Control.Monad (join)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NE
import qualified Control.Lens as Lens

-- * Simple formatters

emptyWhiteStarToNewline :: White'Star Char () -> White'Star Char ()
emptyWhiteStarToNewline = (newline <>)

-- | Changes an empty 'White'Star' to a single space.  Makes no
-- change to any other 'White'Star'.
emptyWhiteStarToSingleSpace :: White'Star Char () -> White'Star Char ()
emptyWhiteStarToSingleSpace = Lens.over Lens._Wrapped' f
  where
    f sq
      | Seq.null sq = Seq.singleton cWhite'Space
      | otherwise = sq

-- | Adds a preceding newline to a 'White'Star', regardless of its
-- contents.
addNewlineToWhiteStar :: White'Star Char () -> White'Star Char ()
addNewlineToWhiteStar (White'Star sq)
  = White'Star (cWhite'Newline `Lens.cons` sq)

-- | Adds a newline to the front of the 'FileItemP', but
-- only if the 'FileItem' is not a 'Comment'.
precedeNonCommentWithBlankLine :: FileItemP Char () -> FileItemP Char ()
precedeNonCommentWithBlankLine fip@(FileItemP ws it) = case it of
  FileItem'Price _ -> FileItemP (emptyWhiteStarToNewline ws) it
  FileItem'Transaction _ -> FileItemP (emptyWhiteStarToNewline ws) it
  _ -> fip

-- | Indents, but only if the 'White'Star' is empty.
indent :: Int -> White'Star Char () -> White'Star Char ()
indent i ws@(White'Star sq)
  | not (Seq.null sq) = ws
  | otherwise = newline <> spaces i

-- | Adds the given number of spaces, but only if the 'White'Star is empty.
addSpaces :: Int -> White'Star Char () -> White'Star Char ()
addSpaces i ws@(White'Star sq)
  | not (Seq.null sq) = ws
  | otherwise = spaces i


-- * Separating comments

data NonComment t a
  = NonComment'Transaction (White'Star t a, Transaction t a)
  | NonComment'Price (White'Star t a, Price t a)
  deriving Show

Lens.makePrisms ''NonComment

splitFileItem
  :: FileItemP t a
  -> Either (White'Star t a, Comment t a) (NonComment t a)
splitFileItem (FileItemP ws fi) = case fi of
  FileItem'Price p -> Right (NonComment'Price (ws, p))
  FileItem'Comment c -> Left (ws, c)
  FileItem'Transaction t -> Right (NonComment'Transaction (ws, t))


groupsToFileItems
  :: SeqUtil.Groups (White'Star t a, Comment t a) (NonComment t a)
  -> Seq (FileItemP t a)
groupsToFileItems groups = g1 <> g2 <> g3
  where
    g1 = fmap unNonComment . SeqUtil._leaders $ groups
    g2 = join . fmap unPair . SeqUtil._middle $ groups
    g3 = fmap unComment . SeqUtil._trailers $ groups
    unComment (ws, com) = FileItemP ws (FileItem'Comment com)
    unNonComment (NonComment'Transaction (ws, tr))
      = FileItemP ws (FileItem'Transaction tr)
    unNonComment (NonComment'Price (ws, pr))
      = FileItemP ws (FileItem'Price pr)
    unPair (neComments, neNonComments) = seqComs <> seqNonComs
      where
        seqComs = NE.nonEmptySeqToSeq . fmap unComment $ neComments
        seqNonComs = NE.nonEmptySeqToSeq . fmap unNonComment $ neNonComments

-- | Accesses the leading whitespace in the trailing group (if it is there.)
trailerLeadingWhitespace
  :: Lens.Traversal' (Groups (White'Star a b, Comment a b) (NonComment a b))
                     (White'Star a b)
trailerLeadingWhitespace
  = SeqUtil.trailers
  . Lens._Cons
  . Lens._1
  . Lens._1

-- | Accesses the leading 'White'Star' of every group of comments
-- (other than the first one) in the middle of the 'Groups'.
middleLeadingWhitespacesIfNotFirst
  :: Lens.Traversal' (Groups (White'Star a b, Comment a b) (NonComment a b))
                  (White'Star a b)
middleLeadingWhitespacesIfNotFirst
  = SeqUtil.middle
  . Lens._Cons
  . Lens._2
  . traverse
  . Lens._1
  . NE.fore
  . Lens._1

-- | Accesses the leading 'White'Star' of the first group of comments
-- in the 'Groups'.
middleLeadingCommentGroupWhitespaces
  :: Lens.Traversal' (Groups (White'Star a b, Comment a b) (NonComment a b))
                     (White'Star a b)
middleLeadingCommentGroupWhitespaces
  = SeqUtil.middle
  . Lens._Cons
  . Lens._1
  . Lens._1
  . NE.fore
  . Lens._1

-- | Given a 'SeqUtil.Groups', add whitespace before blocks of
-- comments.
--
-- Adds nothing before the 'SeqUtil._leaders', as all these are not
-- comments.
--
-- Adds whitespace before each comment group in 'SeqUtil._middle' that
-- is not the in the first item in 'SeqUtil._middle'.
--
-- Adds whitespace before the first comment group in
-- 'SeqUtil._middle', but only if 'SeqUtil._leaders' is not empty.
-- This way, whitespace is added before the first comment group, but
-- only if it is not the very first thing in the file.
--
-- Adds whitespace before 'SeqUtil._trailers' only if it is not empty.
addWhitespaceBeforeCommentGroups
  :: Groups (White'Star Char (), Comment Char ()) (NonComment Char ())
  -> Groups (White'Star Char (), Comment Char ()) (NonComment Char ())
addWhitespaceBeforeCommentGroups
  = Lens.over middleLeadingWhitespacesIfNotFirst emptyWhiteStarToNewline
  . Lens.over trailerLeadingWhitespace emptyWhiteStarToNewline
  . addToFirstCommentGroupIfNoLeaders
  where
    addToFirstCommentGroupIfNoLeaders groups
      | Seq.null (SeqUtil._leaders groups) = groups
      | otherwise = Lens.over
          middleLeadingCommentGroupWhitespaces emptyWhiteStarToNewline groups

-- | Traverses a sequence of 'FileItemP'.  For each comment,
-- adds a newline to the front, but only if the preceding 'FileItem'
-- is not a comment.
--
-- The idea is that comments already have a trailing newline in the
-- grammar.  However, if we have a single comment by itself, we want
-- to have a blank line preceding that comment.  Also, if we have a
-- block of comments, we want to have only a single blank line
-- preceding that block of comments, but no additional newlines
-- between each comment line.
--
-- Does not add whitespace before the first comment block.
precedeCommentBlocksWithBlankLine
  :: FileItemP'Star Char ()
  -> FileItemP'Star Char ()
precedeCommentBlocksWithBlankLine (FileItemP'Star sq)
  = FileItemP'Star
  . groupsToFileItems
  . addWhitespaceBeforeCommentGroups
  . SeqUtil.groupEithers
  . fmap splitFileItem
  $ sq

-- * Indenting transactions

indentTransaction :: Int -> Transaction Char () -> Transaction Char ()
indentTransaction i
  = Lens.over whitesBetweenTopLineAndPostings (indent i)
  . Lens.over whitesBetweenOpenCurlyAndFirstPosting (addSpaces 1)
  . Lens.over whitesBetweenPostingEndAndSemi (indent i)
  . Lens.over whitesBetweenSemiAndPosting (addSpaces 1)
  . Lens.over whitesAfterPostingList (indent i)

-- * Formatting times

-- | True if a 'N0'59' is zero.
isZeroN0'59 :: N0'59 t a -> Bool
isZeroN0'59 (N0'59 (D0'5'Zero _) (D0'9'Zero _)) = True
isZeroN0'59 _ = False

-- | True if a 'Minutes' is zero.
isZeroMinutes :: Minutes t a -> Bool
isZeroMinutes (Minutes x) = isZeroN0'59 x

isZeroSeconds :: Seconds t a -> Bool
isZeroSeconds (Seconds x) = isZeroN0'59 x

isZeroN0'19 :: N0'19 t a -> Bool
isZeroN0'19 (N0'19 (D0'1'Opt Nothing) (D0'9'Zero _)) = True
isZeroN0'19 (N0'19 (D0'1'Opt (Just (D0'1'Zero _))) (D0'9'Zero _)) = True
isZeroN0'19 _ = False

isZeroHours :: Hours t a -> Bool
isZeroHours (Hours'N0'19 n0) = isZeroN0'19 n0
isZeroHours _ = False

-- | True if a time zone is UTC.
isUTC :: Zone t a -> Bool
isUTC (Zone _ hrs _ mins) = isZeroHours hrs && isZeroMinutes mins

-- | True if a 'WhitesZone'Opt' is either 'Nothing' or UTC.
isUTCOrBlank :: WhitesZone'Opt t a -> Bool
isUTCOrBlank wzo = Lens.has nothing wzo || Lens.andOf utc wzo
  where
    nothing = Lens._Wrapped' . Lens._Nothing
    utc = Lens._Wrapped' . Lens._Just . r'WhitesZone'1'Zone . Lens.to isUTC

-- | True if a time is midnight.
isMidnight :: Time t a -> Bool
isMidnight (Time hrs _ mins maySecs)
  = isZeroHours hrs && isZeroMinutes mins
  && Lens.andOf getSecs maySecs
  where
    getSecs
      = Lens._Wrapped'
      . Lens._Just
      . r'ColonSeconds'1'Seconds
      . Lens.to isZeroSeconds

-- | Deletes the time zone if it is UTC.
removeUTCZone :: WhitesZone'Opt t a -> WhitesZone'Opt t a
removeUTCZone wz
  | Lens.andOf get wz = WhitesZone'Opt Nothing
  | otherwise = wz
  where
    get = Lens._Wrapped' . Lens._Just . r'WhitesZone'1'Zone . Lens.to isUTC

-- | Deletes the time and zone if the time is midnight and the zone is
-- UTC.
removeMidnightUTC :: TimeAndMayZone'Opt t a -> TimeAndMayZone'Opt t a
removeMidnightUTC tmz
  | Lens.andOf testUtc tmz && Lens.andOf testMidnight tmz
      = TimeAndMayZone'Opt Nothing
  | otherwise = tmz
  where
    testUtc = Lens._Wrapped' . Lens._Just . r'TimeAndMayZone'1'WhitesZone'Opt
      . Lens.to isUTCOrBlank
    testMidnight = Lens._Wrapped' . Lens._Just . r'TimeAndMayZone'0'WhitesTime
      . r'WhitesTime'1'Time . Lens.to isMidnight

-- * Filtering posting fields

filterPostingFieldP'Star
  :: (PostingField t a -> Bool)
  -> PostingFieldP'Star t a
  -> PostingFieldP'Star t a
filterPostingFieldP'Star pd = Lens.over Lens._Wrapped'
  $ Seq.filter (Lens.view (r'PostingFieldP'1'PostingField . Lens.to pd))

filterPostingFieldsP
  :: (PostingField t a -> Bool)
  -> PostingFields t a
  -> PostingFields t a
filterPostingFieldsP pd (PostingFields p1 ps) = case Lens.uncons sq' of
  Nothing -> PostingFields p1 ps
  Just (x, xs)
    | pd (_r'PostingFieldP'1'PostingField x)
        -> PostingFields p1 (PostingFieldP'Star sq')
    | otherwise -> PostingFields (_r'PostingFieldP'1'PostingField x)
        (PostingFieldP'Star xs)
  where
    PostingFieldP'Star sq' = filterPostingFieldP'Star pd ps

-- | Sorts a 'PostingFields'. This destroys any existing whitespace.
sortPostingFields
  :: (PostingField Char () -> PostingField Char () -> Ordering)
  -> PostingFields Char ()
  -> PostingFields Char ()
sortPostingFields by
  = toPostingFields
  . sortNonEmptySeq by
  . fromPostingFields
  where
    toPostingFields (NE.NonEmptySeq p1 ps)
      = PostingFields p1 . PostingFieldP'Star
        . fmap (PostingFieldP cWhite'Plus) $ ps
    fromPostingFields (PostingFields p1 (PostingFieldP'Star ps))
      = NE.NonEmptySeq p1 (fmap _r'PostingFieldP'1'PostingField ps)

quotedStringIsEmpty :: QuotedString t a -> Bool
quotedStringIsEmpty (QuotedString _ (QuotedChar'Star sq) _)
  = Seq.null sq

anyStringIsEmpty :: AnyString t a -> Bool
anyStringIsEmpty as = case as of
  AnyString'UnquotedString _ -> False
  AnyString'QuotedString qs -> quotedStringIsEmpty qs

-- | Sorts posting fields into this order:
--
-- 0. number
-- 1. flag
-- 2. account
-- 3. fitid
-- 4. tags
-- 5. uid
-- 6. ofxTrn
-- 7. origDay
standardPostingFieldSort
  :: PostingFields Char ()
  -> PostingFields Char ()
standardPostingFieldSort = sortPostingFields $ comparing $ \c -> case c of
  PostingField'Number _ -> 0
  PostingField'Flag _ -> 1
  PostingField'Account _ -> 2
  PostingField'Fitid _ -> 3
  PostingField'Tags _ -> 4
  PostingField'Uid _ -> 5
  PostingField'OfxTrn _ -> 6
  PostingField'OrigDate _ -> 7

-- | Formats an entire file:
--
-- * uses the given level of indentation to indent transactions.
-- Makes no change to transactions already indented.
--
-- * Precedes every comment group with a newline.
--
-- * Precedes every price and transaction, except for the first item
-- in the file, with a newline.
--
-- * Inserts whitespace for readability.
--
-- * Removes midnight UTC times from top lines
--
-- * Removes midnight UTC times from the @origDate@ field
--
-- * Removes UTC time zones from top lines
--
-- * Removes UTC time zones from the @origDate@ field
--
-- * Sorts posting fields
--
-- This list may grow in the future.

formatWholeFile
  :: Int
  -- ^ Indentation level
  -> WholeFile Char ()
  -> WholeFile Char ()
formatWholeFile i
  = Lens.over allTransactions (indentTransaction i)
  . Lens.over r'WholeFile'0'FileItemP'Star precedeCommentBlocksWithBlankLine
  . Lens.over tailFileItemP precedeNonCommentWithBlankLine
  . Lens.over sepWholeFile (addSpaces 1)
  . Lens.over (topLineDates . r'DateTimeZone'1'TimeAndMayZone'Opt)
      removeMidnightUTC
  . Lens.over (allOrigDate . r'DateTimeZone'1'TimeAndMayZone'Opt)
      removeMidnightUTC
  . Lens.over (topLineDates . zoneInDateTimeZone) removeUTCZone
  . Lens.over (allOrigDate . zoneInDateTimeZone) removeUTCZone
  . Lens.over (allPostings . postingFieldsInPosting)
              standardPostingFieldSort
