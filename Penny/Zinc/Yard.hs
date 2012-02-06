module Penny.Zinc.Yard where

import Data.List.NonEmpty (NonEmpty, nonEmpty, (|:), neHead, neTail)
import Data.List (groupBy, intersperse)
import Data.Monoid.Extra (All(All, appAll), Any(Any, appAny))
import Data.Monoid (mappend)

{- Shunting yard steps.

1. Parse command line into operands and operators.

2. Any time we see two operands in a row, insert an "and" operator in
between them.

-}

newtype Operand a = Operand a
                    deriving Show

data OpType a =
  Unary (a -> a)
  | Binary (a -> a -> a)

instance Show (OpType a) where
  show (Unary _) = "<OpType unary>"
  show (Binary _) = "<OpType binary>"

type Precedence = Int

data Operator a = Operator Precedence (OpType a)
                  deriving Show

data InputItem a =
  OpenParen
  | CloseParen
  | IINotParen (NotParen a)
  deriving Show

type NotParen a = Either (Operator a) a

type OutputItem a = NotParen a



--data PostfixOutput = PostfixOutput [

{-
data Operator =
  Not
  | And
  | Or
  | OpenParen
  | CloseParen
  deriving (Eq, Ord, Show)

data Item a = ItOperator Operator
              | ItOperand (Operand a)
              deriving Show

type Line a = [Item a]

insertAnds :: Line a -> Line a
insertAnds is = let
  p i1 i2 = case (i1, i2) of
    (ItOperand _, ItOperand _) -> True
    _ -> False
  grouped = groupBy p is
  grouper = intersperse (ItOperator And)
  in concatMap grouper grouped

type OperandStack a = [Operand a]

type OperatorStack = [Operator]

parseItem ::
  OperandStack a
  -> OperatorStack
  -> NonEmpty (Item a)
  -> Maybe (OperandStack a, OperatorStack, [Item a])
parseItem ds ts is = case neHead is of
  (ItOperand o) ->
    return (o:ds, ts, neTail is)
  (ItOperator o) -> do
    (ds', ts') <- parseHigherOperators o ds ts
    return (ds', o:ts', neTail is)

parseHigherOperators ::
  Operator
  -> OperandStack a
  -> OperatorStack
  -> Maybe (OperandStack a, OperatorStack)
parseHigherOperators o ds ts = case ts of
  [] -> return (ds, ts)
  (x:xs) ->
    if x >= o
    then do
      ds' <- applyOperator x ds
      return (ds', xs)
    else return (ds, ts)

applyOperator ::
  Operator
  -> OperandStack a
  -> Maybe (OperandStack a)
applyOperator o os = case o of
  Not -> case os of
    [] -> Nothing
    ((Operand x):xs) -> let
      f' = not . x
      in Just ((Operand f') : xs)
  And -> case os of
    [] -> Nothing
    (x:[]) -> Nothing
    ((Operand x1):(Operand x2):xs) -> let
      f' = appAll $ mappend (All x1) (All x2)
      in Just ((Operand f') : xs)
  Or -> case os of
    [] -> Nothing
    (x:[]) -> Nothing
    ((Operand x1):(Operand x2):xs) -> let
      f' = appAny $ mappend (Any x1) (Any x2)
      in Just ((Operand f') : xs)
         

-}
