{-# LANGUAGE OverloadedStrings #-}

-- | Trees of predicates.

module Penny.Steel.Predtree
  ( Pdct(..)
  , always
  , never
  , (&&&)
  , (|||)
  , Level
  , IndentAmt
  , showPdct
  , evaluate
  ) where

import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>), mconcat)

-- | A tree of predicates.
data Pdct a
  = And Text [Pdct a]
  | Or Text [Pdct a]
  | Not Text (Pdct a)
  | Operand Text (a -> Bool)

-- | Returns a tree that is always True.
always :: Pdct a
always = Operand "always True" (const True)

-- | Returns a tree that is always False.
never :: Pdct a
never = Operand "always False" (const False)

(&&&) :: Pdct a -> Pdct a -> Pdct a
(&&&) x y = And "and" [x, y]
infixr 3 &&&

(|||) :: Pdct a -> Pdct a -> Pdct a
(|||) x y = Or "or" [x, y]
infixr 2 |||

type Level = Int
type IndentAmt = Int

indent :: IndentAmt -> Level -> Text -> Text
indent amt lvl s = X.replicate (lvl * amt) " " <> s <> "\n"

showPdct :: IndentAmt -> Level -> Pdct a -> Text
showPdct amt lvl pd = case pd of
  And l ls -> indent amt lvl l
              <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Or l ls -> indent amt lvl l
             <> mconcat (map (showPdct amt (lvl + 1)) ls)
  Not l t -> indent amt lvl l <> showPdct amt (lvl + 1) t
  Operand l _ -> indent amt lvl l

labelBool :: Text -> Bool -> Text
labelBool t b = trueFalse <> " " <> t
  where
    trueFalse = if b then "[TRUE] " else "[FALSE]"

evaluate :: IndentAmt -> a -> Level -> Pdct a -> (Bool, Text)
evaluate i a lvl pd = case pd of
  And l ps -> let (resBool, resTxt) = evalAnd i a (lvl + 1) ps
                  txt = indent i lvl (labelBool l resBool)
                        <> resTxt
              in (resBool, txt)
  Or l ps -> let (resBool, resTxt) = evalOr i a (lvl + 1) ps
                 txt = indent i lvl (labelBool l resBool)
                        <> resTxt
             in (resBool, txt)
  Not l p -> let (resBool, resTxt) = evaluate i a (lvl + 1) p
                 txt = indent i lvl (labelBool l (not resBool))
                        <> resTxt
             in (resBool, txt)
  Operand l p -> let res = p a
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
           in go xs (not res, acc <> cTxt)

evalOr :: IndentAmt -> a -> Level -> [Pdct a] -> (Bool, Text)
evalOr i a l ts = (found, txt)
  where
    (found, txt) = go ts (False, X.empty)
    go [] p = p
    go (x:xs) (fnd, acc) =
      if fnd
      then (fnd, acc <> indent i l "(short circuit)")
      else let (res, cTxt) = evaluate i a l x
           in go xs (res, acc <> cTxt)

