-- | Postfix, or RPN, expression parsing.
--
-- This module parses RPN expressions where the operands are
-- predicates and the operators are one of @and@, @or@, or @not@,
-- where @and@ and @or@ are binary and @not@ is unary.
module Penny.Steel.Expressions.RPN where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl
import qualified Penny.Steel.Predtree as P
import Penny.Steel.Predtree ((&&&), (|||))

data RPNToken a
  = TokOperand (P.Pdct a)
  -- ^ The Text describes the operand, for use in error messages and
  -- for printing a diagnostic tree.
  | TokOperator Operator

data Operator
  = OpAnd
  | OpOr
  | OpNot
  deriving Show

data RPNError a
  = InsufficientStack Operator
  | EmptyStack
  | FullStack [P.Pdct a]
  deriving Show

pushOperand :: P.Pdct a -> [P.Pdct a] -> [P.Pdct a]
pushOperand p ts = p : ts

pushOperator
  :: Operator
  -> [P.Pdct a]
  -> Ex.Exceptional (RPNError a) [P.Pdct a]
pushOperator o ts = case o of
  OpAnd -> case ts of
    x:y:zs -> return $ (y &&& x) : zs
    _ -> Ex.throw $ InsufficientStack OpAnd
  OpOr -> case ts of
    x:y:zs -> return $ (y ||| x) : zs
    _ -> Ex.throw $ InsufficientStack OpOr
  OpNot -> case ts of
    x:zs -> return $ P.not x : zs
    _ -> Ex.throw $ InsufficientStack OpNot

pushToken
  :: [P.Pdct a]
  -> RPNToken a
  -> Ex.Exceptional (RPNError a) [P.Pdct a]
pushToken ts t = case t of
  TokOperand p -> return $ pushOperand p ts
  TokOperator o -> pushOperator o ts

pushTokens
  :: Fdbl.Foldable f
  => f (RPNToken a)
  -> Ex.Exceptional (RPNError a) (P.Pdct a)
pushTokens ts = do
  trees <- Fdbl.foldlM pushToken [] ts
  case trees of
    [] -> Ex.throw $ EmptyStack
    x:[] -> return x
    xs -> Ex.throw $ FullStack xs

