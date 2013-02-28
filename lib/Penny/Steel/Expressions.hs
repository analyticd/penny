module Penny.Steel.Expressions
  ( ExprDesc(..)
  , ExprError(..)
  , parseExpression
  , I.InfixToken(..)
  , I.Paren(..)
  , R.Token(..)
  , R.Operator(..)
  , R.Tree(..)
  , R.RPNError(..)
  ) where

import Data.Either (partitionEithers)
import qualified Penny.Steel.Expressions.Infix as I
import qualified Penny.Steel.Expressions.RPN as R
import qualified Control.Monad.Exception.Synchronous as Ex

data ExprDesc
  = Infix
  | RPN

data ExprError a = UnbalancedParen
                 | RPNErr (R.RPNError a)
                 | ParenInRPN

toksToRPN :: [I.InfixToken a] -> Maybe [R.Token a]
toksToRPN toks
  = let toEither t = case t of
          I.TokRPN tok -> Right tok
          _ -> Left ()
    in case partitionEithers . map toEither $ toks of
        ([], xs) -> return xs
        _ -> Nothing

parseExpression
  :: ExprDesc
  -> [I.InfixToken a]
  -> Ex.Exceptional (ExprError a) (R.Tree a)
parseExpression e toks = do
  rpnToks <- case e of
    Infix -> Ex.fromMaybe UnbalancedParen $ I.createRPN toks
    RPN -> Ex.fromMaybe ParenInRPN $ toksToRPN toks
  Ex.mapException RPNErr $ R.pushTokens rpnToks
