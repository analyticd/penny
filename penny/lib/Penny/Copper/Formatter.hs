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
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>))

-- | Ensures that the 'White'Star' ends with a newline.  Makes no
-- change if the 'White'Star' already ends either with a newline or
-- with a comment (all comments already have a newline built-in).
newlineEnding :: White'Star Char () -> White'Star Char ()
newlineEnding (White'Star ws) = White'Star $ case Lens.unsnoc ws of
  Nothing -> Seq.singleton cWhite'Newline
  Just (_, x) -> case x of
    White'Comment _ -> ws
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

-- | Strip all whitespace, except comments, from a 'White'Star'.
stripNonComments :: White'Star Char () -> White'Star Char ()
stripNonComments = Lens.over Lens._Wrapped'
  (Seq.filter (Lens.is _White'Comment))

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

formatNextPosting
  :: NextPosting Char () -> NextPosting Char ()
formatNextPosting
  = Lens.over (r'NextPosting'0'White'Star . Lens._Wrapped') fmtSpc1
  . Lens.over r'NextPosting'2'White'Star addSpace
  . Lens.over r'NextPosting'3'Posting formatPosting
  where
    fmtSpc1 sq = case Lens.unsnoc sq of
      Nothing -> cWhite'Newline <| cWhite'Space <| cWhite'Space <| Seq.empty
      Just (_, x) -> case x of
        White'Newline _ -> sq
        White'Comment _ -> sq
        _ -> sq |> cWhite'Newline |> cWhite'Space |> cWhite'Space

formatPostings :: Postings Char () -> Postings Char ()
formatPostings = undefined

