module Penny.Zinc.Expressions (
  I.Precedence(Precedence),
  
  I.Associativity(ALeft, ARight),
  
  I.Token(TokUnaryPostfix,
          TokUnaryPrefix,
          TokBinary,
          TokOpenParen,
          TokCloseParen),

  I.Infix(Infix),
  evaluate) where

import Penny.Zinc.Expressions.Infix as I
import Penny.Zinc.Expressions.RPN as R

evaluate :: I.Infix a -> Maybe a
evaluate i = I.infixToRPN i >>= R.process
