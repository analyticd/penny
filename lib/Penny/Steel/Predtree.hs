{-# LANGUAGE OverloadedStrings #-}

-- | Trees of predicates.

module Penny.Steel.Predtree
  ( Pdct(..)
  , always
  , never
  , pdctAnd
  , pdctOr
  , pdctNot
  , operand
  , neverFalse
  , neverTrue
  , (&&&)
  , (|||)
  , Level
  , IndentAmt
  , showPdct
  , evaluate
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat)

type Label = Text

-- | A tree of predicates.
data Pdct a = Pdct Label (Node a)

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
      Nothing -> "[skip] "
      Just bl -> if bl then "[TRUE] " else "[FALSE]"

evaluate :: IndentAmt -> a -> Level -> Pdct a -> (Maybe Bool, Text)
evaluate i a lvl (Pdct l pd) = case pd of
  And ps -> let (resBool, resTxt) = evalAnd i a (lvl + 1) ps
                txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
            in (Just resBool, txt)
  Or ps -> let (resBool, resTxt) = evalOr i a (lvl + 1) ps
               txt = indent i lvl (labelBool l (Just resBool))
                        <> resTxt
           in (Just resBool, txt)
  Not p -> let (childMayBool, childTxt) = evaluate i a (lvl + 1) p
               thisMayBool = fmap not childMayBool
               txt = indent i lvl (labelBool l thisMayBool)
                        <> childTxt
           in (thisMayBool, txt)
  Operand p -> let res = p a
               in (res, indent i lvl (labelBool l res))

evalAnd :: IndentAmt -> a -> Level -> [Pdct a] -> (Bool, Text)
evalAnd i a l ts = (not foundFalse, txt)
  where
    (foundFalse, txt) = go ts (False, X.empty)
    go [] p = p
    go (x:xs) (fndFalse, acc) =
      if fndFalse
      then (fndFalse, acc <> indent i l "(short circuit)")
      else let (res, cTxt) = evaluate i a l x
               fndFalse' = maybe False not res
           in go xs (fndFalse', acc <> cTxt)

evalOr :: IndentAmt -> a -> Level -> [Pdct a] -> (Bool, Text)
evalOr i a l ts = (foundTrue, txt)
  where
    (foundTrue, txt) = go ts (False, X.empty)
    go [] p = p
    go (x:xs) (fnd, acc) =
      if fnd
      then (fnd, acc <> indent i l "(short circuit)")
      else let (res, cTxt) = evaluate i a l x
               fnd' = fromMaybe False res
           in go xs (fnd', acc <> cTxt)

