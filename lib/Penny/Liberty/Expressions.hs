module Penny.Liberty.Expressions (
  I.Precedence(Precedence),
  
  I.Associativity(ALeft, ARight),
  
  I.Token(TokOperand,
          TokUnaryPostfix,
          TokUnaryPrefix,
          TokBinary,
          TokOpenParen,
          TokCloseParen),

  R.Operand(Operand),
  tokAnd,
  tokOr,
  tokNot,
  evaluate) where

import Penny.Liberty.Expressions.Infix as I
import Penny.Liberty.Queue (Queue, enqueue, empty)
import qualified Penny.Liberty.Queue as Q
import Penny.Liberty.Expressions.RPN as R

import Data.List (intersperse, groupBy)

-- | Tokens should be enqueued from left to right.
evaluate :: Queue (I.Token a) -> Maybe a
evaluate i = I.infixToRPN i >>= R.process

-- | Takes the list of tokens and gets the predicate to use.
getPredicate :: 
  [I.Token (a -> Bool)]
  -> Maybe (a -> Bool)
getPredicate ls =
  if null ls then Just (const True) else evaluate q where
    q = foldl (flip Q.enqueue) Q.empty (insertAddTokens ls)

-- | Operands that are not separated by operators are assumed to be
-- joined with an and operator; this function adds the and operators.
insertAddTokens :: [I.Token (a -> Bool)]
                   -> [I.Token (a -> Bool)]
insertAddTokens ts = concatMap inserter grouped where
  inserter = intersperse tokAnd
  grouped = groupBy f ts
  f x y = case (x, y) of
    (I.TokOperand _, I.TokOperand _) -> True
    _ -> False

-- | An And token which is left associative with precedence 3.
tokAnd :: I.Token (a -> Bool)
tokAnd = I.TokBinary (I.Precedence 3) I.ALeft f where
  f x y = \a -> x a && y a

-- | An Or token which is left associative with precedence 2.
tokOr :: I.Token (a -> Bool)
tokOr = I.TokBinary (I.Precedence 2) I.ALeft f where
  f x y = \a -> x a || y a

-- | A unary prefix Not token with precedence 4.
tokNot :: I.Token (a -> Bool)
tokNot = I.TokUnaryPrefix (I.Precedence 4) (not .)
