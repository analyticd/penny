{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Manipulates whitespace for the
-- types in "Penny.Copper.Types".
--
-- Additive formatters only.  These formatters never destroy data.
--
-- In Copper's grammar, generally whitespace precedes items.
-- One exception is the comment, which already contains a newline at the end.
-- Functions in this file add whitespace between items.  This is
-- the job of 'precedeNonCommentWithBlankLine' and

module Penny.Copper.ArrangeWhites where

import Penny.Copper.Types
import Penny.Copper.Optics
import Penny.Copper.Copperize

import qualified Control.Lens as Lens
import Control.Monad (join)
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NE
import Penny.SeqUtil (Groups)
import qualified Penny.SeqUtil as SeqUtil

{-

-- | Changes an empty 'White'Star' to a single space.  Makes no
-- change to any other 'White'Star'.
emptyWhiteStarToSingleSpace :: White'Star Char () -> White'Star Char ()
emptyWhiteStarToSingleSpace = Lens.over Lens._Wrapped' f
  where
    f sq
      | Seq.null sq = Seq.singleton cWhite'Space
      | otherwise = sq

-- | Changes an empty 'White'Star' to a single newline.  Makes no
-- change to any other 'White'Star'.
emptyWhiteStarToNewline :: White'Star Char () -> White'Star Char ()
emptyWhiteStarToNewline = Lens.over Lens._Wrapped' f
  where
    f sq
      | Seq.null sq = Seq.singleton cWhite'Newline
      | otherwise = sq

-- | Adds a preceding newline to a 'White'Star', regardless of its
-- contents.
addNewlineToWhiteStar :: White'Star Char () -> White'Star Char ()
addNewlineToWhiteStar (White'Star sq)
  = White'Star (cWhite'Newline `Lens.cons` sq)

-- | Adds two newlines to a 'White'Star'.
insertBlankLine :: White'Star Char () -> White'Star Char ()
insertBlankLine (White'Star sq)
  = White'Star (cWhite'Newline `Lens.cons` cWhite'Newline `Lens.cons` sq)

-- | Formats a 'Forest' so that a single space separates each
-- 'Tree'.
separateForestWithSpaces :: Forest Char () -> Forest Char ()
separateForestWithSpaces (Forest t1 ts) = Forest t1 (separateNextTreeStarWithSpaces ts)

separateNextTreeWithSpaces :: NextTree Char () -> NextTree Char ()
separateNextTreeWithSpaces (NextTree sp1 com sp2 t)
  = NextTree sp1 com (emptyWhiteStarToSingleSpace sp2) t

separateNextTreeStarWithSpaces :: NextTree'Star Char () -> NextTree'Star Char ()
separateNextTreeStarWithSpaces
  = Lens.over Lens._Wrapped' (fmap separateNextTreeWithSpaces)

-- | Formats the 'Forest' within the 'BracketedForest'.
separateBracketedForestWithSpaces :: BracketedForest Char () -> BracketedForest Char ()
separateBracketedForestWithSpaces
  = Lens.over r'BracketedForest'2'Forest separateForestWithSpaces

-- | Adds a preceding space to the 'WhitesBracketedForest'Opt'.
precedeWhitesBracketedForestOptWithSpace
  :: WhitesBracketedForest'Opt Char ()
  -> WhitesBracketedForest'Opt Char ()
precedeWhitesBracketedForestOptWithSpace = Lens.over s emptyWhiteStarToSingleSpace
  where
    s = Lens._Wrapped' . Lens._Just . r'WhitesBracketedForest'0'White'Star

formatTopLine :: TopLine Char () -> TopLine Char ()
formatTopLine = Lens.over Lens._Wrapped' separateForestWithSpaces

formatTrioMaybeForest :: TrioMaybeForest Char () -> TrioMaybeForest Char ()
formatTrioMaybeForest = Lens.over
  r'TrioMaybeForest'1'WhitesBracketedForest'Opt
  precedeWhitesBracketedForestOptWithSpace

formatPosting :: Posting Char () -> Posting Char ()
formatPosting = Lens.over _Posting'TrioMaybeForest formatTrioMaybeForest
  . Lens.over _Posting'BracketedForest separateBracketedForestWithSpaces

-- | Adds a newline and two spaces to a 'White'Star', but only if
-- the 'White'Star' is currently empty.
addIndent :: White'Star Char () -> White'Star Char ()
addIndent (White'Star ws)
  | Seq.null ws = White'Star . Seq.fromList $
    [ cWhite'Newline, cWhite'Space, cWhite'Space ]
  | otherwise = White'Star ws

indentNextPosting
  :: NextPosting Char () -> NextPosting Char ()
indentNextPosting
  = Lens.over r'NextPosting'0'White'Star addIndent
  . Lens.over r'NextPosting'2'White'Star emptyWhiteStarToSingleSpace
  . Lens.over r'NextPosting'3'Posting formatPosting

indentNextPosting'Star
  :: NextPosting'Star Char ()
  -> NextPosting'Star Char ()
indentNextPosting'Star = Lens.over (Lens._Wrapped' . Lens.mapped)
  indentNextPosting

indentPostingList :: PostingList Char () -> PostingList Char ()
indentPostingList
  = Lens.over r'PostingList'0'White'Star emptyWhiteStarToSingleSpace
  . Lens.over r'PostingList'1'Posting formatPosting
  . Lens.over r'PostingList'2'NextPosting'Star indentNextPosting'Star

indentPostingList'Opt :: PostingList'Opt Char () -> PostingList'Opt Char ()
indentPostingList'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) indentPostingList

indentPostings :: Postings Char () -> Postings Char ()
indentPostings
  = Lens.over r'Postings'1'PostingList'Opt indentPostingList'Opt
  . Lens.over r'Postings'2'White'Star addIndent

indentWhitesPostings :: WhitesPostings Char () -> WhitesPostings Char ()
indentWhitesPostings
  = Lens.over r'WhitesPostings'0'White'Star addIndent
  . Lens.over r'WhitesPostings'1'Postings indentPostings

indentWhitesPostings'Opt
  :: WhitesPostings'Opt Char ()
  -> WhitesPostings'Opt Char ()
indentWhitesPostings'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) indentWhitesPostings

formatTopLineMaybePostings
  :: TopLineMaybePostings Char ()
  -> TopLineMaybePostings Char ()
formatTopLineMaybePostings
  = Lens.over r'TopLineMaybePostings'0'TopLine formatTopLine
  . Lens.over r'TopLineMaybePostings'1'WhitesPostings'Opt
              indentWhitesPostings'Opt

-- | Formats a 'NextPosting', without indentation.
formatNextPosting
  :: NextPosting Char () -> NextPosting Char ()
formatNextPosting
  = Lens.over r'NextPosting'0'White'Star emptyWhiteStarToNewline
  . Lens.over r'NextPosting'2'White'Star emptyWhiteStarToSingleSpace
  . Lens.over r'NextPosting'3'Posting formatPosting


-- | Formats a 'NextPosting'Star', without indentation.
formatNextPosting'Star
  :: NextPosting'Star Char ()
  -> NextPosting'Star Char ()
formatNextPosting'Star = Lens.over (Lens._Wrapped' . Lens.mapped)
  formatNextPosting


-- | Formats a 'PostingList', without indentation.
formatPostingList :: PostingList Char () -> PostingList Char ()
formatPostingList
  = Lens.over r'PostingList'0'White'Star emptyWhiteStarToSingleSpace
  . Lens.over r'PostingList'1'Posting formatPosting
  . Lens.over r'PostingList'2'NextPosting'Star formatNextPosting'Star


-- | Formats a 'PostingList'Opt', without indentation.
formatPostingList'Opt :: PostingList'Opt Char () -> PostingList'Opt Char ()
formatPostingList'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) formatPostingList


-- | Format postings, without indentation.
--
-- This is used to format a 'Postings' which does not have a top line,
-- so you would not want it to be indented.
formatPostings
  :: Postings Char ()
  -> Postings Char ()
formatPostings
  = Lens.over r'Postings'1'PostingList'Opt formatPostingList'Opt
  . Lens.over r'Postings'2'White'Star emptyWhiteStarToNewline

-- | Adds appropriate whitespace to the interior of a 'Transaction'.
--
-- The top line is never indented.  A transaction with a top line and
-- postings has indented postings.  A transaction with no top line
-- does not have its postings indented.
formatTransaction
  :: Transaction Char ()
  -> Transaction Char ()
formatTransaction
  = Lens.over _Transaction'TopLineMaybePostings formatTopLineMaybePostings
  . Lens.over _Transaction'Postings formatPostings

-- # Prices
formatWhitesTime :: WhitesTime Char () -> WhitesTime Char ()
formatWhitesTime
  = Lens.over r'WhitesTime'0'White'Star emptyWhiteStarToSingleSpace

formatWhitesZone :: WhitesZone Char () -> WhitesZone Char ()
formatWhitesZone
  = Lens.over r'WhitesZone'0'White'Star emptyWhiteStarToSingleSpace

formatWhitesTime'Opt :: WhitesTime'Opt Char () -> WhitesTime'Opt Char ()
formatWhitesTime'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) formatWhitesTime

formatWhitesZone'Opt :: WhitesZone'Opt Char () -> WhitesZone'Opt Char ()
formatWhitesZone'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) formatWhitesZone

-- | Adds appropriate whitespace to the interior of a 'Price'.
formatPrice :: Price Char () -> Price Char ()
formatPrice
  = Lens.over r'Price'1'White'Star emptyWhiteStarToSingleSpace
  . Lens.over r'Price'3'WhitesTime'Opt formatWhitesTime'Opt
  . Lens.over r'Price'4'WhitesZone'Opt formatWhitesZone'Opt
  . Lens.over r'Price'5'White'Star emptyWhiteStarToSingleSpace
  . Lens.over r'Price'7'White'Star emptyWhiteStarToSingleSpace

-- | Adds a newline to the front of the 'WhitesFileItem', but
-- only if the 'FileItem' is not a 'Comment'.
precedeNonCommentWithBlankLine :: WhitesFileItem Char () -> WhitesFileItem Char ()
precedeNonCommentWithBlankLine wfi@(WhitesFileItem ws it) = case it of
  FileItem'Price _ -> WhitesFileItem (insertBlankLine ws) it
  FileItem'Transaction _ -> WhitesFileItem (insertBlankLine ws) it
  _ -> wfi

data NonComment t a
  = NonComment'Transaction (White'Star t a, Transaction t a)
  | NonComment'Price (White'Star t a, Price t a)
  deriving Show

Lens.makePrisms ''NonComment

splitFileItem
  :: WhitesFileItem t a
  -> Either (White'Star t a, Comment t a) (NonComment t a)
splitFileItem (WhitesFileItem ws fi) = case fi of
  FileItem'Price p -> Right (NonComment'Price (ws, p))
  FileItem'Comment c -> Left (ws, c)
  FileItem'Transaction t -> Right (NonComment'Transaction (ws, t))

groupsToFileItems
  :: SeqUtil.Groups (White'Star t a, Comment t a) (NonComment t a)
  -> Seq (WhitesFileItem t a)
groupsToFileItems groups = g1 <> g2 <> g3
  where
    g1 = fmap unNonComment . SeqUtil._leaders $ groups
    g2 = join . fmap unPair . SeqUtil._middle $ groups
    g3 = fmap unComment . SeqUtil._trailers $ groups
    unComment (ws, com) = WhitesFileItem ws (FileItem'Comment com)
    unNonComment (NonComment'Transaction (ws, tr))
      = WhitesFileItem ws (FileItem'Transaction tr)
    unNonComment (NonComment'Price (ws, pr))
      = WhitesFileItem ws (FileItem'Price pr)
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
  :: Lens.Setter' (Groups (White'Star a b, Comment a b) (NonComment a b))
                  (White'Star a b)
middleLeadingWhitespacesIfNotFirst
  = SeqUtil.middle
  . Lens._Cons
  . Lens._2
  . Lens.mapped
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
  = Lens.over middleLeadingWhitespacesIfNotFirst insertBlankLine
  . Lens.over trailerLeadingWhitespace insertBlankLine
  . addToFirstCommentGroupIfNoLeaders
  where
    addToFirstCommentGroupIfNoLeaders groups
      | Seq.null (SeqUtil._leaders groups) = groups
      | otherwise = Lens.over
          middleLeadingCommentGroupWhitespaces insertBlankLine groups

-- | Traverses a sequence of 'WhitesFileItem'.  For each comment,
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
  :: Seq (WhitesFileItem Char ())
  -> Seq (WhitesFileItem Char ())
precedeCommentBlocksWithBlankLine
  = groupsToFileItems
  . addWhitespaceBeforeCommentGroups
  . SeqUtil.groupEithers
  . fmap splitFileItem

-- | Gets the 'Seq' of 'WhitesFileItem' that are after the first
-- 'WhitesFileItem'.

tailWhitesFileItemsInWholeFile
  :: Lens.Traversal' (WholeFile t a) (Seq (WhitesFileItem t a))
tailWhitesFileItemsInWholeFile = r'WholeFile'0'WhitesFileItem'Star
  . Lens._Wrapped' . Lens._Cons . Lens._2

-- | Gets the first file item in the 'WholeFile'.
firstFileItem :: Lens.Traversal' (WholeFile t a) (FileItem t a)
firstFileItem = r'WholeFile'0'WhitesFileItem'Star
  . Lens._Wrapped' . Lens._Cons . Lens._1
  . r'WhitesFileItem'1'FileItem

-- | Maps over every 'FileItem' in the 'WholeFile'.
allFileItems :: Lens.Setter' (WholeFile t a) (FileItem t a)
allFileItems
  = r'WholeFile'0'WhitesFileItem'Star
  . Lens._Wrapped'
  . Lens.mapped
  . r'WhitesFileItem'1'FileItem

-- | Maps over every 'Transaction' in the 'WholeFile'.
allTransactions :: Lens.Setter' (WholeFile t a) (Transaction t a)
allTransactions = allFileItems . _FileItem'Transaction

-- | Maps over every 'Price' in the 'WholeFile'.
allPrices :: Lens.Setter' (WholeFile t a) (Price t a)
allPrices = allFileItems . _FileItem'Price

-- | Maps over every 'Comment' in the 'WholeFile'.
allComments :: Lens.Setter' (WholeFile t a) (Comment t a)
allComments = allFileItems . _FileItem'Comment

-- | Formats every item in the file.  Does not add preceding
-- whitespace to the first item, but formats it otherwise.
formatWholeFile
  :: WholeFile Char ()
  -> WholeFile Char ()
formatWholeFile
  = Lens.over (tailWhitesFileItemsInWholeFile . Lens.mapped)
              precedeNonCommentWithBlankLine
  . Lens.over (r'WholeFile'0'WhitesFileItem'Star . Lens._Wrapped')
              precedeCommentBlocksWithBlankLine
  . Lens.over allTransactions formatTransaction
  . Lens.over allPrices formatPrice

-- | Appends two 'WholeFile' together, while adding a single newline
-- between them.
appendWholeFileWithSeparator
  :: WholeFile Char () -> WholeFile Char () -> WholeFile Char ()
appendWholeFileWithSeparator w1 w2 = w1 `mappend` (blank `mappend` w2)
  where
    blank = WholeFile mempty (cWhite'Star 1 cWhite'Newline)
-}
