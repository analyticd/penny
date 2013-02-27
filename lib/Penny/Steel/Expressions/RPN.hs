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

data Error a
  = InsufficientStack Operator
  | EmptyStack
  | FullStack [Tree a]

pushOperand :: String -> (a -> Bool) -> [Tree a] -> [Tree a]
pushOperand s p ts = EOperand s p : ts

pushOperator :: Operator -> [Tree a] -> Ex.Exceptional (Error a) [Tree a]
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

pushToken :: [Tree a] -> Token a -> Ex.Exceptional (Error a) [Tree a]
pushToken ts t = case t of
  TokOperand d p -> return $ pushOperand d p ts
  TokOperator o -> pushOperator o ts

pushTokens
  :: Fdbl.Foldable f
  => f (Token a)
  -> Ex.Exceptional (Error a) (Tree a)
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


showTree :: Tree a -> String
showTree t = showTree' 0 t

indentation :: Int
indentation = 4

indent :: Int -> String -> String
indent i s = replicate (i * indentation) ' ' ++ s ++ "\n"

showTree' :: Int -> Tree a -> String
showTree' l t = case t of
  EAnd x y -> indent l "and" ++ showTree' (l + 1) x
                             ++ showTree' (l + 1) y
  EOr x y -> indent l "or" ++ showTree' (l + 1) x
                           ++ showTree' (l + 1) y
  ENot x -> indent l "not" ++ showTree' (l + 1) x
  EOperand s _ -> indent l s
