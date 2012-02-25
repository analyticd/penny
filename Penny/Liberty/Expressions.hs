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
  evaluate) where

import Penny.Liberty.Expressions.Infix as I
import Data.Queue (Queue, enqueue, empty)
import Penny.Liberty.Expressions.RPN as R

evaluate :: Queue (I.Token a) -> Maybe a
evaluate i = I.infixToRPN i >>= R.process

--
-- Testing
--

_expr :: Queue (I.Token Int)
_expr = foldl (flip enqueue) empty [
  I.TokOpenParen
  , I.TokOperand 3
  , _add
  , I.TokOperand 4
  , I.TokCloseParen
  , _mult
  , I.TokOperand 5
  , _add
  , I.TokOperand 8
  , _div
  , I.TokOperand 2
  ]


_add :: Num a => I.Token a
_add = I.TokBinary (I.Precedence 5) I.ALeft (+)

_mult :: Num a => I.Token a
_mult = I.TokBinary (I.Precedence 6) I.ALeft (*)

_div :: Integral a => I.Token a
_div = I.TokBinary (I.Precedence 6) I.ALeft div

