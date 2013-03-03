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

type Label = Text

-- | A tree of predicates.
data Pdct a = Pdct Label (Node a)

data Node a
  = And [Pdct a]
  | Or [Pdct a]
  | Not (Pdct a)
  | Operand (a -> Maybe Bool)

pdctAnd :: Text -> [Pdct a] -> Pdct a
pdctAnd t = Pdct t . And

pdctOr :: Text -> [Pdct a] -> Pdct a
pdctOr t = Pdct t . Or

pdctNot :: Text -> Pdct a -> Pdct a
pdctNot t = Pdct t . Not

operand :: Text -> (a -> Bool) -> Pdct a
operand t = Pdct t . Operand

-- | Turns an existing Pdct to a waffling one. If the underlying
-- predicate returns Just True, the new Pdct also returns Just
-- True. Otherwise, the Pdct returns Nothing. (Waffles can't say no.)
waffle :: Pdct a -> Pdct a
waffle p@(Pdct l n) =
  let calc f a = case f a of
        Nothing -> Nothing
        Just b -> if b then Just True else Nothing
  in Pdct t $ case n of
      And ps -> case eval

ePdct :: a -> Pdct a -> Maybe Bool
ePdct (Pdct t 

waffle p@(Pdct l n) = case n of
  Operand f ->
    let f' a = case f a of
          Nothing -> Nothing
          Just b -> if b then Just True else Nothing
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

labelBool :: Text -> Bool -> Text
labelBool t b = trueFalse <> " " <> t
  where
    trueFalse = if b then "[TRUE] " else "[FALSE]"

evaluate :: IndentAmt -> a -> Level -> Pdct a -> (Maybe Bool, Text)
evaluate i a lvl (Pdct l pd) = case pd of
  And ps -> let (resBool, resTxt) = evalAnd i a (lvl + 1) ps
                txt = indent i lvl (labelBool l resBool)
                        <> resTxt
            in (resBool, txt)
  Or ps -> let (resBool, resTxt) = evalOr i a (lvl + 1) ps
               txt = indent i lvl (labelBool l resBool)
                        <> resTxt
           in (resBool, txt)
  Not p -> let (resBool, resTxt) = evaluate i a (lvl + 1) p
               txt = indent i lvl (labelBool l (not resBool))
                        <> resTxt
           in (resBool, txt)
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

