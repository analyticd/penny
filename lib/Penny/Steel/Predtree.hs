{-# LANGUAGE OverloadedStrings #-}

-- | Trees of predicates.

module Penny.Steel.Predtree
  ( Pdct(..)
  , always
  , never
  , pdctAnd
  , pdctOr
  , pdctNot
  , pNot
  , operand
  , neverFalse
  , neverTrue
  , (&&&)
  , (|||)
  , Level
  , IndentAmt
  , showPdct
  , rename
  , eval
  , evaluate
  ) where

import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat)

type Label = Text

-- | A tree of predicates.
data Pdct a = Pdct Label (Node a)

-- | Renames the top level of the Pdct. The function you pass will be
-- applied to the old name.
rename :: (Text -> Text) -> Pdct a -> Pdct a
rename f (Pdct l n) = Pdct (f l) n

data Node a
  = And [Pdct a]
  -- ^ None of the Pdct in list may be Just False. An empty list or
  -- list with only Nothing is Just True.

  | Or [Pdct a]
  -- ^ At least one of the Pdct in the list must be Just True. An
  -- empty lis or list with only Nothing is Just False.

  | Not (Pdct a)
  -- ^ Just True is Just False and vice versa; Nothing remains Nothing.

  | Operand (a -> Maybe Bool)

pdctAnd :: Text -> [Pdct a] -> Pdct a
pdctAnd t = Pdct t . And

pdctOr :: Text -> [Pdct a] -> Pdct a
pdctOr t = Pdct t . Or

pdctNot :: Text -> Pdct a -> Pdct a
pdctNot t = Pdct t . Not

pNot :: Pdct a -> Pdct a
pNot = pdctNot (X.pack "not")

-- | Creates a new operand. The Pdct is Just True or Just False, never
-- Nothing.
operand :: Text -> (a -> Bool) -> Pdct a
operand t = Pdct t . Operand . fmap Just

-- | Turns an existing Pdct to one that never says False. If the
-- underlying predicate returns Just True, the new Pdct also returns
-- Just True. Otherwise, the Pdct returns Nothing.  Has no effect on
-- non-Operand Pdct.
neverFalse :: Pdct a -> Pdct a
neverFalse p@(Pdct l n) = case n of
  Operand f ->
    let f' a = case f a of
          Nothing -> Nothing
          Just b -> if b then Just True else Nothing
    in Pdct l (Operand f')
  _ -> p

-- | Turns an existing Pdct to one that never says True. If the
-- underlying predicate returns Just False, the new Pdct also returns
-- Just False. Otherwise, the Pdct returns Nothing.  Has no effect on
-- non-Operand Pdct.
neverTrue :: Pdct a -> Pdct a
neverTrue p@(Pdct l n) = case n of
  Operand f ->
    let f' a = case f a of
          Nothing -> Nothing
          Just b -> if not b then Just False else Nothing
    in Pdct l (Operand f')
  _ -> p


-- | Returns a tree that is always True.
always :: Pdct a
always = Pdct "always True" (Operand (const (Just True)))

-- | Returns a tree that is always False.
never :: Pdct a
never = Pdct "always False" (Operand (const (Just False)))

(&&&) :: Pdct a -> Pdct a -> Pdct a
(&&&) x y = Pdct "and" (And [x, y])
infixr 3 &&&

(|||) :: Pdct a -> Pdct a -> Pdct a
(|||) x y = Pdct "or" (Or [x, y])
infixr 2 |||

type Level = Int
type IndentAmt = Int

indent :: IndentAmt -> Level -> Text -> Text
indent amt lvl s = X.replicate (lvl * amt) " " <> s <> "\n"

showPdct :: IndentAmt -> Level -> Pdct a -> Text
showPdct amt lvl (Pdct l pd) = case pd of
  And ls -> indent amt lvl l
            <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Or ls -> indent amt lvl l
           <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Not t -> indent amt lvl l <> showPdct amt (lvl + 1) t
  Operand _ -> indent amt lvl l

labelBool :: Text -> Maybe Bool -> Text
labelBool t b = trueFalse <> " " <> t
  where
    trueFalse = case b of
      Nothing -> "[discard] "
      Just bl -> if bl then "[TRUE]    " else "[FALSE]   "

type ShowDiscards = Bool

-- | Evaluates a Pdct.
eval :: Pdct a -> a -> Maybe Bool
eval (Pdct _ n) a = case n of
  And ps -> Just . and . catMaybes $ [flip eval a] <*> ps
  Or ps -> Just . or . catMaybes $ [flip eval a] <*> ps
  Not p -> fmap not $ eval p a
  Operand f -> f a

-- | Verbosely evaluates a Pdct.
evaluate
  :: IndentAmt
  -- ^ Indent each level by this many spaces.

  -> ShowDiscards
  -- ^ If True, show discarded test results; otherwise, hide
  -- them.

  -> a
  -- ^ The subject to evaluate

  -> Level
  -- ^ How many levels deep in the tree we are. Start at level 0. This
  -- determines the level of indentation.
  -> Pdct a
  -> (Maybe Bool, Text)
evaluate i sd a lvl (Pdct l pd) = case pd of

  And ps -> let (resBool, resTxt) = evalAnd i sd a (lvl + 1) ps
                txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
            in (Just resBool, txt)

  Or ps -> let (resBool, resTxt) = evalOr i sd a (lvl + 1) ps
               txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
           in (Just resBool, txt)

  Not p -> let (childMayBool, childTxt) = evaluate i sd a (lvl + 1) p
               thisMayBool = fmap not childMayBool
               thisTxt = indent i lvl (labelBool l thisMayBool)
               txt = if sd || isJust thisMayBool
                     then thisTxt <> childTxt else X.empty
           in (thisMayBool, txt)

  Operand p -> let res = p a
                   txt = indent i lvl (labelBool l res)
               in (res, if sd || isJust res then txt else X.empty)

evalAnd :: IndentAmt -> ShowDiscards -> a
        -> Level -> [Pdct a] -> (Bool, Text)
evalAnd i sd a l ts = (not foundFalse, txt)
  where
    (foundFalse, txt) = go ts (False, X.empty)
    go [] p = p
    go (x:xs) (fndFalse, acc) =
      if fndFalse
      then (fndFalse, acc <> indent i l "(short circuit)")
      else let (res, cTxt) = evaluate i sd a l x
               fndFalse' = maybe False not res
           in go xs (fndFalse', acc <> cTxt)

evalOr :: IndentAmt -> ShowDiscards -> a
       -> Level -> [Pdct a] -> (Bool, Text)
evalOr i sd a l ts = (foundTrue, txt)
  where
    (foundTrue, txt) = go ts (False, X.empty)
    go [] p = p
    go (x:xs) (fnd, acc) =
      if fnd
      then (fnd, acc <> indent i l "(short circuit)")
      else let (res, cTxt) = evaluate i sd a l x
               fnd' = fromMaybe False res
           in go xs (fnd', acc <> cTxt)

