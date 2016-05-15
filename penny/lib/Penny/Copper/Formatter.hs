{-# LANGUAGE FlexibleContexts #-}
-- | Manipulates whitespace and other formatting details for the
-- types in "Penny.Copper.Types".
--
-- Additive formatters only.  These formatters never destroy data.

module Penny.Copper.Formatter where

import Penny.Copper.Types
import Penny.Copper.Optics
import Penny.Copper.Copperize

import qualified Control.Lens as Lens
import qualified Control.Lens.Extras as Lens (is)
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>))

-- | Ensures that the 'White'Star' ends with a newline.
newlineEnding :: White'Star Char () -> White'Star Char ()
newlineEnding (White'Star ws) = White'Star $ case Lens.unsnoc ws of
  Nothing -> Seq.singleton cWhite'Newline
  Just (_, x) -> case x of
    White'Newline _ -> ws
    White'Space _ -> ws `Lens.snoc` cWhite'Newline
    White'Tab _ -> ws `Lens.snoc` cWhite'Newline


-- | Changes an empty 'White'Star' to a single space.  Makes no
-- change to any other 'White'Star'.
addSpace :: White'Star Char () -> White'Star Char ()
addSpace = Lens.over Lens._Wrapped' f
  where
    f sq
      | Seq.null sq = Seq.singleton cWhite'Space
      | otherwise = sq

-- | Changes an empty 'White'Star' to a single newline.  Makes no
-- change to any other 'White'Star'.
addNewline :: White'Star Char () -> White'Star Char ()
addNewline = Lens.over Lens._Wrapped' f
  where
    f sq
      | Seq.null sq = Seq.singleton cWhite'Newline
      | otherwise = sq

-- | Ensures that each item in the 'WhitesFileItem'Star' is separated by
-- at least one newline or comment.  Does not alter the first
-- 'WhitesFileItem'.
separateWhitesFileItems
  :: WhitesFileItem'Star Char ()
  -> WhitesFileItem'Star Char ()
separateWhitesFileItems = Lens.over s newlineEnding
  where
    s = Lens._Wrapped' . Lens._Cons . Lens._2 . Lens.mapped . 
      r'WhitesFileItem'0'White'Star 


-- | Formats a 'Forest' so that a single space separates each
-- 'Tree'.
formatForest :: Forest Char () -> Forest Char ()
formatForest (Forest t1 ts) = Forest t1 (formatNextTree'Star ts)

formatNextTree :: NextTree Char () -> NextTree Char ()
formatNextTree (NextTree sp1 com sp2 t)
  = NextTree (addSpace sp1) com (addSpace sp2) t

formatNextTree'Star :: NextTree'Star Char () -> NextTree'Star Char ()
formatNextTree'Star = Lens.over Lens._Wrapped' (fmap formatNextTree)

-- | Formats the 'Forest' within the 'BracketedForest'.  Does
formatBracketedForest :: BracketedForest Char () -> BracketedForest Char ()
formatBracketedForest = Lens.over r'BracketedForest'2'Forest formatForest

-- | Adds a preceding space to the 'WhitesBracketedForest'Opt'.
formatWhitesBracketedForest'Opt
  :: WhitesBracketedForest'Opt Char ()
  -> WhitesBracketedForest'Opt Char ()
formatWhitesBracketedForest'Opt = Lens.over s addSpace
  where
    s = Lens._Wrapped' . Lens._Just . r'WhitesBracketedForest'0'White'Star

formatScalarMaybeForest
  :: ScalarMaybeForest Char () -> ScalarMaybeForest Char ()
formatScalarMaybeForest = Lens.over
  r'ScalarMaybeForest'1'WhitesBracketedForest'Opt
  formatWhitesBracketedForest'Opt

formatForestMaybeScalar
  :: ForestMaybeScalar Char () -> ForestMaybeScalar Char ()
formatForestMaybeScalar
  = Lens.over r'ForestMaybeScalar'0'BracketedForest formatBracketedForest
  . Lens.over s addSpace
  where
    s = r'ForestMaybeScalar'1'WhitesScalar'Opt
      . Lens._Wrapped' . Lens._Just . r'WhitesScalar'0'White'Star

formatTree :: Tree Char () -> Tree Char ()
formatTree = Lens.over _Tree'ScalarMaybeForest formatScalarMaybeForest
  . Lens.over _Tree'ForestMaybeScalar formatForestMaybeScalar

formatTopLine :: TopLine Char () -> TopLine Char ()
formatTopLine = Lens.over Lens._Wrapped' formatForest

formatTrioMaybeForest :: TrioMaybeForest Char () -> TrioMaybeForest Char ()
formatTrioMaybeForest = Lens.over
  r'TrioMaybeForest'1'WhitesBracketedForest'Opt
  formatWhitesBracketedForest'Opt

formatPosting :: Posting Char () -> Posting Char ()
formatPosting = Lens.over _Posting'TrioMaybeForest formatTrioMaybeForest
  . Lens.over _Posting'BracketedForest formatBracketedForest

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
  . Lens.over r'NextPosting'2'White'Star addSpace
  . Lens.over r'NextPosting'3'Posting formatPosting

indentNextPosting'Star
  :: NextPosting'Star Char ()
  -> NextPosting'Star Char ()
indentNextPosting'Star = Lens.over (Lens._Wrapped' . Lens.mapped)
  indentNextPosting

indentPostingList :: PostingList Char () -> PostingList Char ()
indentPostingList
  = Lens.over r'PostingList'0'White'Star addSpace
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
  = Lens.over r'NextPosting'0'White'Star addNewline
  . Lens.over r'NextPosting'2'White'Star addSpace
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
  = Lens.over r'PostingList'0'White'Star addSpace
  . Lens.over r'PostingList'1'Posting formatPosting
  . Lens.over r'PostingList'2'NextPosting'Star formatNextPosting'Star


-- | Formats a 'PostingList'Opt', without indentation.
formatPostingList'Opt :: PostingList'Opt Char () -> PostingList'Opt Char ()
formatPostingList'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) formatPostingList


-- | Format postings, without indentation.
formatPostings
  :: Postings Char ()
  -> Postings Char ()
formatPostings
  = Lens.over r'Postings'1'PostingList'Opt formatPostingList'Opt
  . Lens.over r'Postings'2'White'Star addNewline

formatTransaction
  :: Transaction Char ()
  -> Transaction Char ()
formatTransaction
  = Lens.over _Transaction'TopLineMaybePostings formatTopLineMaybePostings
  . Lens.over _Transaction'Postings formatPostings

-- # Prices
formatWhitesTime :: WhitesTime Char () -> WhitesTime Char ()
formatWhitesTime
  = Lens.over r'WhitesTime'0'White'Star addSpace

formatWhitesZone :: WhitesZone Char () -> WhitesZone Char ()
formatWhitesZone
  = Lens.over r'WhitesZone'0'White'Star addSpace

formatWhitesTime'Opt :: WhitesTime'Opt Char () -> WhitesTime'Opt Char ()
formatWhitesTime'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) formatWhitesTime

formatWhitesZone'Opt :: WhitesZone'Opt Char () -> WhitesZone'Opt Char ()
formatWhitesZone'Opt
  = Lens.over (Lens._Wrapped' . Lens._Just) formatWhitesZone

formatPrice :: Price Char () -> Price Char ()
formatPrice
  = Lens.over r'Price'1'White'Star addSpace
  . Lens.over r'Price'3'WhitesTime'Opt formatWhitesTime'Opt
  . Lens.over r'Price'4'WhitesZone'Opt formatWhitesZone'Opt
  . Lens.over r'Price'5'White'Star addSpace
  . Lens.over r'Price'7'White'Star addSpace

formatFileItem :: FileItem Char () -> FileItem Char ()
formatFileItem
  = Lens.over _FileItem'Price formatPrice
  . Lens.over _FileItem'Transaction formatTransaction

-- | Adds a newline to the front of the  the 'WhitesFileItem', but
-- only if the 'FileItem' is not a 'Comment'.
formatWhitesFileItem :: WhitesFileItem Char () -> WhitesFileItem Char ()
formatWhitesFileItem (WhitesFileItem ws it) = case it of
  FileItem'Price _ -> WhitesFileItem (addNewline ws) (formatFileItem it)
  FileItem'Transaction _ -> WhitesFileItem (addNewline ws) (formatFileItem it)
  FileItem'Comment _ -> WhitesFileItem ws (formatFileItem it)

-- | Traverses a sequence of 'WhitesFileItem'.  For each comment,
-- adds a newline to the front, but only if the preceding 'FileItem'
-- is not a comment.
formatCommentWhitesFileItems
  :: Seq (WhitesFileItem Char ())
  -> Seq (WhitesFileItem Char ())
formatCommentWhitesFileItems sq = case Lens.uncons sq of
  Nothing -> Seq.empty
  Just (x, xs) -> case Lens.uncons xs of
    Nothing -> Seq.singleton x
    Just (y, ys)
      | not (isComment x) && isComment y -> x <| preNewline y <|
          formatCommentWhitesFileItems ys
      | otherwise -> x <| formatCommentWhitesFileItems xs
      where
        isComment = Lens.is _FileItem'Comment
          . Lens.view r'WhitesFileItem'1'FileItem
        preNewline = Lens.over r'WhitesFileItem'0'White'Star addNewline

-- | Formats every item.
formatWhitesFileItem'Star
  :: WhitesFileItem'Star Char ()
  -> WhitesFileItem'Star Char ()
formatWhitesFileItem'Star
  = Lens.over (Lens._Wrapped' . Lens.mapped) formatWhitesFileItem
  . Lens.over Lens._Wrapped' formatCommentWhitesFileItems

-- | Formats every item in the file.  Does not add whitespace to the
-- first item, but formats it otherwise.
formatWholeFile
  :: WholeFile Char ()
  -> WholeFile Char ()
formatWholeFile
  = Lens.over getWfis formatWhitesFileItem
  . Lens.over getSeqWfi formatCommentWhitesFileItems
  . Lens.over getFirstFi formatFileItem
  where
    getWfis = r'WholeFile'0'WhitesFileItem'Star
      . Lens._Wrapped' . Lens._Cons . Lens._2 . Lens.mapped
    getSeqWfi = r'WholeFile'0'WhitesFileItem'Star
      . Lens._Wrapped' . Lens._Cons . Lens._2
    getFirstFi = r'WholeFile'0'WhitesFileItem'Star
      . Lens._Wrapped' . Lens._Cons . Lens._1
      . r'WhitesFileItem'1'FileItem
