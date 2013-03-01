{-# LANGUAGE OverloadedStrings #-}

-- | Postfix, or RPN, expression parsing.
--
-- This module parses RPN expressions where the operands are
-- predicates and the operators are one of @and@, @or@, or @not@,
-- where @and@ and @or@ are binary and @not@ is unary.
module Penny.Steel.Expressions.RPN where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid ((<>))

data Token a
  = TokOperand Text (a -> Bool)
  -- ^ The Text describes the operand, for use in error messages and
  -- for printing a diagnostic tree.
  | TokOperator Operator

data Operator
  = OpAnd
  | OpOr
  | OpNot

data Tree a
  = EAnd (Tree a) (Tree a)
  | EOr (Tree a) (Tree a)
  | ENot (Tree a)
  | EOperand Text (a -> Bool)

-- | Returns a tree that is always True.
always :: Tree a
always = EOperand "always True" (const True)

-- | Returns a tree that is always False.
never :: Tree a
never = EOperand "always False" (const False)

(&&&) :: Tree a -> Tree a -> Tree a
(&&&) = EAnd
infixr 3 &&&

(|||) :: Tree a -> Tree a -> Tree a
(|||) = EOr
infixr 2 |||

data RPNError a
  = InsufficientStack Operator
  | EmptyStack
  | FullStack [Tree a]

pushOperand :: Text -> (a -> Bool) -> [Tree a] -> [Tree a]
pushOperand s p ts = EOperand s p : ts

pushOperator :: Operator -> [Tree a] -> Ex.Exceptional (RPNError a) [Tree a]
pushOperator o ts = case o of
  OpAnd -> case ts of
    x:y:zs -> return $ EAnd y x : zs
    _ -> Ex.throw $ InsufficientStack OpAnd
  OpOr -> case ts of
    x:y:zs -> return $ EOr y x : zs
    _ -> Ex.throw $ InsufficientStack OpOr
  OpNot -> case ts of
    x:zs -> return $ ENot x : zs
    _ -> Ex.throw $ InsufficientStack OpNot

pushToken :: [Tree a] -> Token a -> Ex.Exceptional (RPNError a) [Tree a]
pushToken ts t = case t of
  TokOperand d p -> return $ pushOperand d p ts
  TokOperator o -> pushOperator o ts

pushTokens
  :: Fdbl.Foldable f
  => f (Token a)
  -> Ex.Exceptional (RPNError a) (Tree a)
pushTokens ts = do
  trees <- Fdbl.foldlM pushToken [] ts
  case trees of
    [] -> Ex.throw $ EmptyStack
    x:[] -> return x
    xs -> Ex.throw $ FullStack xs

evalTree :: Tree a -> a -> Bool
evalTree t a = case t of
  EAnd x y -> evalTree x a && evalTree y a
  EOr x y -> evalTree x a || evalTree y a
  ENot x -> not $ evalTree x a
  EOperand _ p -> p a

type Level = Int
type IndentAmt = Int

indent :: IndentAmt -> Level -> Text -> Text
indent amt lvl s = X.replicate (lvl * amt) " " <> s <> "\n"

showTree :: IndentAmt -> Level -> Tree a -> Text
showTree amt lvl t = case t of
  EAnd x y -> indent amt lvl "and" <> showTree amt (lvl + 1) x
                                   <> showTree amt (lvl + 1) y
  EOr x y -> indent amt lvl "or" <> showTree amt (lvl + 1) x
                                 <> showTree amt (lvl + 1) y
  ENot x -> indent amt lvl "not" <> showTree amt (lvl + 1) x
  EOperand s _ -> indent lvl amt s

labelBool :: Text -> Bool -> Text
labelBool t b = trueFalse <> t
  where
    trueFalse = if b then "[TRUE] " else "[FALSE]"

verboseEval :: IndentAmt -> a -> Level -> Tree a -> (Bool, Text)
verboseEval amt a lvl t =
  let skip = indent amt (lvl + 1) "short circuit"
      eval = verboseEval amt a (lvl + 1)
      idt = indent amt lvl
  in case t of
      EAnd x y ->
        let l1 = idt (labelBool "and" r)
            (bx, l2) = eval x
            (by, l3) = eval y
            r = bx && by
        in (r, l1 <> l2 <> if not bx then l3 else skip)
      EOr x y ->
        let l1 = idt (labelBool "or" r)
            (bx, l2) = eval x
            (by, l3) = eval y
            r = bx || by
        in (r, l1 <> l2 <> if bx then l3 else skip)
      ENot x ->
        let l1 = idt (labelBool "not" r)
            (bx, l2) = eval x
            r = not bx
        in (not bx, l1 <> l2)
      EOperand s p ->
        let r = p a
        in (r, indent amt lvl (labelBool s r))
