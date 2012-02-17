module Penny.Cabin.TextFormat (
  Lines(Lines, unLines),
  Words(Words, unWords),
  wordWrap) where

import qualified Data.Foldable as F
import Data.Sequence ((|>), ViewR((:>)))
import qualified Data.Sequence as S
import qualified Data.Text as X

data Lines = Lines { unLines :: S.Seq Words } deriving Show
data Words = Words { unWords :: S.Seq X.Text } deriving Show

-- | Wraps a sequence of words into a sequence of lines, where each
-- line is no more than a given maximum number of characters long.
--
-- /This function is partial/. It will call 'error' if the maximum
-- number of characters per line is less than 1.
--
-- An individual word will be split across multiple lines only if that
-- word is too long to fit into a single line. No hyphenation is done;
-- the word is simply broken across two lines.
wordWrap :: F.Foldable f => Int -> f X.Text -> Lines
wordWrap uncheckedL = F.foldl f (Lines S.empty) where
  l = if uncheckedL < 1
      then error "wordWrap: length must be at least 1"
      else uncheckedL
  f (Lines sws) w = let
    (back, ws) = case S.viewr sws of
      S.EmptyR -> (S.empty, Words S.empty)
      (b :> x) -> (b, x)
    in case addWord l ws w of
      (Just ws') -> Lines $ back |> ws'
      Nothing ->
        if X.length w > l
        then addPartialWords l (Lines sws) w
        else Lines (back |> ws |> (Words (S.singleton w)))

lenWords :: Words -> Int
lenWords (Words s) = case S.length s of
  0 -> 0
  l -> (F.sum . fmap X.length $ s) + (l - 1)

-- | Adds a word to a Words, but only if it will not make the Words
-- exceed the given length.
addWord :: Int -> Words -> X.Text -> Maybe Words
addWord l (Words ws) w =
  let words' = Words (ws |> w)
  in if lenWords words' > l
     then Nothing
     else Just words'

-- | Adds a word to a Words. If the word is too long to fit, breaks it
-- and adds the longest portion possible. Returns the new Words, and a
-- Text with the part of the word that was not added (if any; if all
-- of the word was added, return an empty Text.)
addPartialWord :: Int -> Words -> X.Text -> (Words, X.Text)
addPartialWord l (Words ws) t = case addWord l (Words ws) t of
  (Just ws') -> (ws', X.empty)
  Nothing ->
    let maxChars = case S.length ws of
          0 -> l
          x -> case l of
            1 -> 0
            len -> l - lenWords (Words ws) - 1
        (begin, end) = X.splitAt maxChars t
    in (Words (if X.null begin then ws else ws |> begin), end)

addPartialWords :: Int -> Lines -> X.Text -> Lines
addPartialWords l (Lines wsq) t = let
  (back, ws) = case S.viewr wsq of
    S.EmptyR -> (S.empty, Words S.empty)
    (b :> x) -> (b, x)
  r@(rw, rt) = addPartialWord l ws t
  in if X.null rt
     then Lines (back |> rw)
     else addPartialWords l (Lines (back |> rw |> Words (S.empty))) rt

-- | Takes a list of words and shortens it so that it fits in the
-- space allotted. You specify the minimum length for each word, x. It
-- will shorten the farthest left word first, until it is only x
-- characters long; then it will shorten the next word until it is
-- only x characters long, etc. This proceeds until all words are just
-- x characters long. Then words are shortened to one
-- character. Then the leftmost words are deleted as necessary.
--
-- Assumes that the words will be printed with a separator, which
-- matters when lengths are calculated.
--
-- /This function is partial./ If applies 'error' if the space
-- requirement is negative.
shorten :: Int -> Words -> Words
shorten = undefined
