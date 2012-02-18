module Penny.Cabin.TextFormat (
  Lines(Lines, unLines),
  Words(Words, unWords),
  wordWrap,
  Target(Target, unTarget),
  Shortest(Shortest, unShortest),
  shorten) where

import qualified Control.Monad.Trans.State as St
import qualified Data.Foldable as F
import Data.Sequence ((|>), ViewR((:>)), ViewL((:<)))
import qualified Data.Sequence as S
import qualified Data.Text as X
import qualified Data.Traversable as T

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

newtype Target = Target { unTarget :: Int } deriving Show
newtype Shortest = Shortest { unShortest :: Int } deriving Show

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
shorten :: Shortest -> Target -> Words -> Words
shorten (Shortest s) (Target t) wsa@(Words wsq) = let
  nToRemove = max (lenWords wsa - t) 0
  (allWords, nLeft) = shortenUntilOne s nToRemove wsq
  in stripWordsUntil t (Words allWords)

-- | Shorten a word by x characters or until it is y characters long,
-- whichever comes first. Returns the word and the number of
-- characters removed.
shortenUntil :: Int -> Int -> X.Text -> (X.Text, Int)
shortenUntil by shortest t = let
  removable = max (X.length t - shortest) 0
  toRemove = min removable (max by 0)
  prefix = X.length t - toRemove
  in (X.take prefix t, toRemove)

-- | Shortens a word until it is x characters long or by the number of
-- characters indicated in the state, whichever is less. Subtracts the
-- number of characters removed from the state.
shortenSt :: Int -> X.Text -> St.State Int X.Text
shortenSt shortest t = do
  by <- St.get
  let (r, nRemoved) = shortenUntil by shortest t
  St.put (by - nRemoved)
  return r

-- | Shortens each word in a list, from left to right, until a
-- particular number of characters have been reduced or until each
-- word is x characters long, whichever happens first. Returns the new
-- list and the number of characters that still need to be reduced.
shortenEachInList ::
  T.Traversable t
  => Int -- ^ Shortest word length
  -> Int -- ^ Total number to remove
  -> t X.Text
  -> (t X.Text, Int)
shortenEachInList shortest by ts = (r, left) where
  k = T.mapM (shortenSt shortest) ts
  (r, left) = St.runState k by

shortenUntilOne ::
  T.Traversable t
  => Int -- ^ Shortest word length to start with
  -> Int -- ^ Total number of characters to remove
  -> t X.Text
  -> (t X.Text, Int)
shortenUntilOne shortest by ts = let
  r@(ts', left) = shortenEachInList shortest by ts
  in if shortest == 1 || left == 0
     then r
     else shortenUntilOne (pred shortest) left ts'

-- | Eliminates words until the length of the words, as indicated by
-- lenWords, is less than or equal to the value given.
stripWordsUntil :: Int -> Words -> Words
stripWordsUntil i wsa@(Words ws) = case S.viewl ws of
  S.EmptyL -> Words (S.empty)
  (_ :< rest) ->
    if lenWords wsa <= (max i 0)
    then wsa
    else stripWordsUntil (max i 0) (Words rest)

  
--
-- Testing
--
_words :: Words
_words = Words . S.fromList . map X.pack $ ws where 
  ws = [ "these", "are", "fragilisticwonderfulgood" ]
--         "good", "", "x", "xy", "xyza",
--         "longlonglongword" ]
