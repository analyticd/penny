module Penny.Steel.Expressions
  ( ExprDesc(..)
  , ExprError(..)
  , Token
  , operand
  , opAnd
  , opOr
  , opNot
  , openParen
  , closeParen
  , parseExpression
  , R.RPNError(..)
  ) where

import Data.Either (partitionEithers)
import qualified Penny.Steel.Expressions.Infix as I
import qualified Penny.Steel.Expressions.RPN as R
import qualified Penny.Steel.Predtree as P
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X

-- | A single type for both RPN tokens and infix tokens.
newtype Token a = Token { unToken :: I.InfixToken a }

operand :: X.Text -> (a -> Bool) -> Token a
operand s p = Token (I.TokRPN (R.TokOperand s p))

opAnd :: Token a
opAnd = Token (I.TokRPN (R.TokOperator R.OpAnd))

opOr :: Token a
opOr = Token (I.TokRPN (R.TokOperator R.OpOr))

opNot :: Token a
opNot = Token (I.TokRPN (R.TokOperator R.OpNot))

openParen :: Token a
openParen = Token (I.TokParen I.Open)

closeParen :: Token a
closeParen = Token (I.TokParen I.Close)

data ExprDesc
  = Infix
  | RPN

data ExprError a = UnbalancedParen
                 | RPNErr (R.RPNError a)
                 | ParenInRPN

toksToRPN :: [Token a] -> Maybe [R.RPNToken a]
toksToRPN toks
  = let toEither t = case unToken t of
          I.TokRPN tok -> Right tok
          _ -> Left ()
    in case partitionEithers . map toEither $ toks of
        ([], xs) -> return xs
        _ -> Nothing

parseExpression
  :: ExprDesc
  -> [Token a]
  -> Ex.Exceptional (ExprError a) (P.Pdct a)
parseExpression e toks = do
  rpnToks <- case e of
    Infix -> Ex.fromMaybe UnbalancedParen
             . I.createRPN
             . map unToken
             $ toks
    RPN -> Ex.fromMaybe ParenInRPN $ toksToRPN toks
  Ex.mapException RPNErr $ R.pushTokens rpnToks
