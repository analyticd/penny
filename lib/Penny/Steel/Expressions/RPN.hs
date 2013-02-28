-- | Postfix, or RPN, expression parsing.
--
-- This module parses RPN expressions where the operands are
-- predicates and the operators are one of @and@, @or@, or @not@,
-- where @and@ and @or@ are binary and @not@ is unary.
module Penny.Steel.Expressions.RPN where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl

data Token a
  = TokOperand String (a -> Bool)
  -- ^ The String describes the operand, for use in error messages and
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
  | EOperand String (a -> Bool)

data RPNError a
  = InsufficientStack Operator
  | EmptyStack
  | FullStack [Tree a]

pushOperand :: String -> (a -> Bool) -> [Tree a] -> [Tree a]
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

indent :: IndentAmt -> Level -> String -> String
indent amt lvl s = replicate (lvl * amt) ' ' ++ s ++ "\n"

showTree :: IndentAmt -> Level -> Tree a -> String
showTree amt lvl t = case t of
  EAnd x y -> indent amt lvl "and" ++ showTree amt (lvl + 1) x
                                   ++ showTree amt (lvl + 1) y
  EOr x y -> indent amt lvl "or" ++ showTree amt (lvl + 1) x
                                 ++ showTree amt (lvl + 1) y
  ENot x -> indent amt lvl "not" ++ showTree amt (lvl + 1) x
  EOperand s _ -> indent lvl amt s

verboseEval :: IndentAmt -> a -> Level -> Tree a -> (Bool, String)
verboseEval amt a lvl t =
  let skip = indent amt (lvl + 1) "short circuit"
      eval = verboseEval amt a (lvl + 1)
      idt = indent amt lvl
  in case t of
      EAnd x y ->
        let l1 = idt "and"
            (bx, l2) = eval x
            (by, l3) = eval y
        in (bx && by, l1 ++ l2 ++ if not bx then l3 else skip)
      EOr x y ->
        let l1 = idt "or"
            (bx, l2) = eval x
            (by, l3) = eval y
        in (bx || by, l1 ++ l2 ++ if bx then l3 else skip)
      ENot x ->
        let l1 = idt "not"
            (bx, l2) = eval x
        in (not bx, l1 ++ l2)
      EOperand s p ->
        let r = p a
            strBool = if r then "[TRUE]  " else "[FALSE] "
        in (r, indent amt lvl (strBool ++ s))
