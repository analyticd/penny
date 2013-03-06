{-# LANGUAGE OverloadedStrings #-}

module Penny.Steel.Predtest where

import Control.Monad.Loops (unfoldrM)
import qualified Control.Monad.Trans.State as St
import Data.Maybe (isJust)
import qualified Data.Text as X
import Data.Text (Text)

import qualified Penny.Steel.Chunk as C
import qualified Penny.Steel.Chunk.Switch as Sw
import qualified Penny.Steel.Predtree as Pt

--
-- Types
--

type Pass = Bool
type Name = Text

-- | A tree of tests. On evaluation of the tree, the name is not shown
-- for tests (it is only shown for groups.) However, the name is used
-- when the tree is displayed statically, without evaluation.
data Tree a = Tree Name (Payload a)

data Payload a
  = Group [Tree a]
  | Test (TestFunc a)

type TestFunc a
  = Pt.IndentAmt
  -> PassVerbosity
  -> FailVerbosity
  -> [a]
  -> Pt.Level
  -> (Pass, [C.Chunk])


group :: Name -> [Tree a] -> Tree a
group n ts = Tree n (Group ts)

test :: Name -> TestFunc a -> Tree a
test n t = Tree n (Test t)

type PassVerbosity = Verbosity
type FailVerbosity = Verbosity

data Verbosity
  = Silent
  -- ^ Show nothing at all

  | PassFail
  -- ^ Show only whether the test passed or failed

  | FalseSubjects
  -- ^ Show subjects that are False

  | TrueSubjects
  -- ^ Show subjects that are True. (This is cumulative, so False
  -- subjects are shown too.)

  | DiscardedSubjects

  | DiscardedPredicates
  -- ^ Show discarded results
  deriving (Eq, Ord, Show)

--
-- Helper functions
--

-- | True if the list is at least n elements long. Less strict than
-- 'length'.
atLeast :: Int -> [a] -> Bool
atLeast x = go 0
  where
    go i [] = i >= x
    go i (_:as) =
      if i >= x
      then True
      else let i' = i + 1 in i' `seq` go i' as


-- | Takes as many elements from a list as necessary until the given
-- number of elements that have Just True as the second element of the
-- pair are found.  Will stop processing if the number of elements is
-- found, but does not discard elements solely because they do not
-- satisfy the predicate. Returns the number of elements found along
-- with the list.
takeCount
  :: Int
  -- ^ Find this many elements
  -> [(a, Maybe Bool)]
  -> ([(a, Maybe Bool)], Int)
takeCount i = flip St.runState 0 . unfoldrM f
  where
    f [] = return Nothing
    f ((a, mb):xs) = do
      c <- St.get
      case () of
        _ | c == i -> return Nothing
          | isTrue mb -> do
              St.put (c + 1)
              return (Just ((a, mb), xs))
          | otherwise -> return (Just ((a, mb), xs))


-- | Determines whether to show a subject, and shows it.
showSubject
  :: (a -> X.Text)
  -> Verbosity
  -> Pt.IndentAmt
  -> Pt.Level
  -> Pt.Pdct a
  -> (a, Maybe Bool)
  -> [C.Chunk]
showSubject swr v i l p (s, b) =
  let (showSubj, showDisc) = isSubjectAndDiscardsShown v b
      renamer txt = X.concat [swr s, " - ", txt]
      renamed = Pt.rename renamer p
  in if showSubj
     then snd $ Pt.evaluate i showDisc s l renamed
     else []

-- | Given a Verbosity and a Maybe Boolean indicating whether a
-- subject is True, False, or a discard, returns whether to show the
-- subject and whether to show the discards contained within the
-- subject.
isSubjectAndDiscardsShown :: Verbosity -> Maybe Bool -> (Bool, Bool)
isSubjectAndDiscardsShown v b = case v of
  Silent -> (False, False)
  PassFail -> (False, False)
  FalseSubjects -> (not . isTrue $ b, False)
  TrueSubjects -> (isJust b, False)
  DiscardedSubjects -> (True, False)
  DiscardedPredicates -> (True, True)


showTestTitle :: Pt.IndentAmt -> Pt.Level -> Name -> Pass -> [C.Chunk]
showTestTitle i l n p = [idt, open, passFail, close, blank, txt, nl]
  where
    passFail = C.chunk ts tf
    idt = C.chunk C.defaultTextSpec (X.replicate (i * l) " ")
    nl = C.chunk C.defaultTextSpec "\n"
    (tf, ts) =
      if p
      then ("PASS", Sw.switchForeground C.color8_f_green
                    C.color256_f_2 C.defaultTextSpec)
      else ("FAIL", Sw.switchForeground C.color8_f_red
                    C.color256_f_1 C.defaultTextSpec)
    open = C.chunk C.defaultTextSpec "["
    close = C.chunk C.defaultTextSpec "]"
    blank = C.chunk C.defaultTextSpec (X.singleton ' ')
    txt = C.chunk C.defaultTextSpec n

isTrue :: Maybe Bool -> Bool
isTrue = maybe False id

--
-- Tests
--

-- | Passes if every subject is True.
eachSubjectMustBeTrue
  :: Name
  -> (a -> Text)
  -> Pt.Pdct a
  -> Tree a
eachSubjectMustBeTrue n swr p = Tree n (Test tf)
  where
    tf i pv fv as lvl = (pass, cks)
      where
        rslts = zip as (map (Pt.eval p) as)
        pass = all (isTrue . snd) rslts
        v = if pass then pv else fv
        cks = tit ++ subjectChunks
        tit = if v == Silent then [] else showTestTitle i lvl n pass
        subjectChunks =
          concatMap (showSubject swr v i (lvl + 1) p) rslts

-- | Passes if at least n subjects are True.
seriesAtLeastN
  :: Name
  -> (a -> X.Text)
  -> Int
  -> Pt.Pdct a
  -> Tree a
seriesAtLeastN n swr count p = Tree n (Test tf)
  where
    tf idnt pv fv as l = (pass, cks)
      where
        (elems, nFound) = takeCount count (zip as (map (Pt.eval p) as))
        pass = nFound >= count
        v = if pass then pv else fv
        cks = tit ++ subjectChunks
        tit = if v == Silent then [] else showTestTitle idnt l n pass
        subjectChunks =
          concatMap (showSubject swr v idnt (l + 1) p) elems

indent :: Pt.IndentAmt -> Pt.Level -> Text -> C.Chunk
indent amt lvl t = C.chunk ts txt
  where
    ts = C.defaultTextSpec
    txt = X.concat [spaces, t, "\n"]
    spaces = X.replicate (amt * lvl) " "

-- | Shows a tree, without evaluating it.
showTree
  :: Pt.IndentAmt
  -> Pt.Level
  -> Tree a
  -> [C.Chunk]
showTree amt l (Tree n p) = indent amt l n : children
  where
    children = case p of
      Group ts -> concatMap (showTree amt l) ts
      Test _ -> []

evalTree
  :: Pt.IndentAmt
  -> Pt.Level
  -> PassVerbosity
  -> FailVerbosity
  -> [a]
  -> Tree a
  -> [Either C.Chunk (Pass, [C.Chunk])]
evalTree i l pv fv as (Tree n p) = case p of
  Test f -> [Right $ f i pv fv as l]
  Group ts -> Left (indent i l n)
              : concatMap (evalTree i (l + 1) pv fv as) ts
