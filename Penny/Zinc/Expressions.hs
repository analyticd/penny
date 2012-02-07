module Penny.Zinc.Expressions (
  I.Precedence(Precedence),
  
  I.Associativity(ALeft, ARight),
  
  I.Token(TokOperand,
          TokUnaryPostfix,
          TokUnaryPrefix,
          TokBinary,
          TokOpenParen,
          TokCloseParen),

  R.Operand(Operand),

  I.Infix(Infix),
  evaluate) where

import Penny.Zinc.Expressions.Infix as I
import Penny.Zinc.Expressions.RPN as R

evaluate :: I.Infix a -> Maybe a
evaluate i = I.infixToRPN i >>= R.process

--
-- Testing
--
_expr :: I.Infix Int
_expr = I.Infix [
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
