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

import qualified Data.Foldable as Fdbl
import Penny.Liberty.Expressions.Infix as I
import Penny.Liberty.Expressions.RPN as R


-- | Tokens should be enqueued from left to right, so that tokens on
-- the left side of the sequence are those at the beginning of the
-- expression.
evaluate ::
  Fdbl.Foldable l
  => l (I.Token a)
  -> Maybe a
evaluate i = I.infixToRPN i >>= R.process

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

--
-- Testing
--
plus :: I.Token Double
plus = I.TokBinary (I.Precedence 4) I.ALeft (+)

minus :: I.Token Double
minus = I.TokBinary (I.Precedence 4) I.ALeft (-)

times :: I.Token Double
times = I.TokBinary (I.Precedence 5) I.ALeft (*)

divide :: I.Token Double
divide = I.TokBinary (I.Precedence 5) I.ALeft (/)

neg :: I.Token Double
neg = I.TokUnaryPrefix (I.Precedence 5) negate
