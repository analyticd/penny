-- | Creates RPN expressions from infix inputs.
--
-- Penny accepts only infix expressions, but RPN expressions are
-- easier to process. This module converts infix expressions to RPN
-- expressions for further processing.
--
-- Uses the shunting-yard algorithm, best described at
-- http://www.chris-j.co.uk/parsing.php (be sure to use the \"click to
-- display\" links).
module Penny.Liberty.Expressions.Infix (

  Precedence(Precedence),
  
  Associativity(ALeft,
                ARight),
  
  Token(TokOperand,
        TokUnaryPostfix,
        TokUnaryPrefix,
        TokBinary,
        TokOpenParen,
        TokCloseParen),
  
  R.Operand(Operand),
  
  infixToRPN
  ) where

import qualified Data.Foldable as Fdbl
import qualified Penny.Liberty.Expressions.RPN as R
import qualified Data.Sequence as Seq
import Data.Sequence((|>), Seq)

newtype Precedence = Precedence Int deriving (Show, Eq, Ord)

data Associativity = ALeft | ARight deriving Show

data Token a =
  TokOperand a
  | TokUnaryPostfix (a -> a)
  | TokUnaryPrefix Precedence (a -> a)
  | TokBinary Precedence Associativity (a -> a -> a)
  | TokOpenParen
  | TokCloseParen

instance (Show a) => Show (Token a) where
  show (TokOperand a) = "<operand " ++ show a ++ ">"
  show (TokUnaryPostfix _) = "<unary postfix>"
  show (TokUnaryPrefix (Precedence i) _) =
    "<unary prefix, precedence " ++ (show i) ++ ">"
  show (TokBinary (Precedence i) a _) =
    "<binary, precedence " ++ show i ++ " "
    ++ show a ++ ">"
  show TokOpenParen = "<OpenParen>"
  show TokCloseParen = "<CloseParen>"

data StackVal a =
  StkUnaryPrefix Precedence (a -> a)
  | StkBinary Precedence (a -> a -> a)
  | StkOpenParen

instance Show (StackVal a) where
  show (StkUnaryPrefix p _) =
    "<unary prefix, " ++ show p ++ ">"
  show (StkBinary p _) =
    "<binary, " ++ show p ++ ">"
  show StkOpenParen = "<OpenParen>"

-- | Converts an infix expression to an RPN expression.
infixToRPN ::
  Fdbl.Foldable l
  => l (Token a)
  -- ^ Input tokens. These should be in the sequence from left to
  -- right in ordinary infix order. The easiest choice is a list,
  -- though you might want to use Data.Sequence if many appends will
  -- be needed to build the sequence.

  -> Maybe (Seq (R.Token a))
  -- ^ The resulting RPN expression. The token type here is a token
  -- from Penny.Liberty.Expressions.RPN, which is a different type
  -- than the Token in this module. Fails if the infix expression
  -- fails to parse for any reason.
infixToRPN ls =
  Fdbl.foldlM processToken ([], Seq.empty) ls
  >>= popRemainingOperators


processToken ::
  ([StackVal a], Seq (R.Token a))
  -> Token a
  -> Maybe ([StackVal a], Seq (R.Token a))
processToken (ss, ts) tok =
  case tok of
    TokOperand a -> Just (ss, processOperand a ts)
    TokUnaryPostfix f ->
      Just (ss, processUnaryPostfix f ts)
    TokUnaryPrefix p f ->
      Just (processUnaryPrefix p f ss, ts)
    TokBinary p a f ->
      Just (processBinary p a f (ss, ts))
    TokOpenParen -> Just (processOpenParen ss, ts)
    TokCloseParen -> processCloseParen (ss, ts)

-- | If a token is an operand, append it to the postfix output.
processOperand :: a -> Seq (R.Token a) -> Seq (R.Token a)
processOperand a sq = sq |> (R.TokOperand (R.Operand a))

-- | If a token is a unary postfix operator, append it to the postfix
-- output.
processUnaryPostfix ::
  (a -> a)
  -> Seq (R.Token a)
  -> Seq (R.Token a)
processUnaryPostfix f sq =
  sq |> (R.TokOperator (R.Unary f))

-- | If a token is a unary prefix operator, push it onto the stack.
processUnaryPrefix ::
  Precedence
  -> (a -> a)
  -> [StackVal a]
  -> [StackVal a]
processUnaryPrefix p f s = (StkUnaryPrefix p f):s

-- | Pops tokens from the stack and appends them to the ouptut, as
-- long as the token at the top of the stack is and operator and its
-- precedence meets the given predicate.
popTokens ::
  (Precedence -> Bool)
  -> ([StackVal a], Seq (R.Token a))
  -> ([StackVal a], Seq (R.Token a))
popTokens f (ss, os) =
  case ss of
    [] -> (ss, os)
    x:xs -> case x of
      StkOpenParen -> (ss, os)
      StkUnaryPrefix p g -> popper (R.Unary g) p
      StkBinary p g -> popper (R.Binary g) p
      where
        popper tok pr =
          if f pr
          then
            let output' = os |> (R.TokOperator tok)
            in popTokens f (xs, output')
          else (ss, os)
  

-- | If the token is a binary operator A, then:
--
-- If A is left associative, while there is an operator B of higher or
-- equal precedence than A at the top of the stack, pop B off the
-- stack and append it to the output.
--
-- If A is right associative, while there is an operator B of higher
-- precedence than A at the top of the stack, pop B off the stack and
-- append it to the output.
--
-- Push A onto the stack.
processBinary ::
  Precedence
  -> Associativity
  -> (a -> a -> a)
  -> ([StackVal a], Seq (R.Token a))
  -> ([StackVal a], Seq (R.Token a))
processBinary p a f pair =
  let pdct = case a of
        ALeft -> (>= p)
        ARight -> (> p)
      (ss, os) = popTokens pdct pair
  in ((StkBinary p f):ss, os)

-- | If the token is an opening parenthesis, push it onto the stack.
processOpenParen :: [StackVal a] -> [StackVal a]
processOpenParen = (StkOpenParen :)

-- | If the token is a closing parenthesis, pop operators off the top
-- of the stack and append them to the output until the operator at
-- the top of the stack is an opening bracket. Pop the opening bracket
-- off the stack.
--
-- Fails if no open paren is found.
processCloseParen ::
  ([StackVal a], Seq (R.Token a))
  -> Maybe ([StackVal a], Seq (R.Token a))
processCloseParen (ss, os) = case ss of
  [] -> Nothing
  (x:xs) ->
    let popper op = processCloseParen (xs, output')
          where
            output' = os |> (R.TokOperator op)
    in case x of
      StkUnaryPrefix _ f -> popper (R.Unary f)        
      StkBinary _ f -> popper (R.Binary f)
      StkOpenParen -> Just (xs, os)

popRemainingOperators ::
  ([StackVal a], Seq (R.Token a))
  -> Maybe (Seq (R.Token a))
popRemainingOperators (s, os) = case s of
  [] -> Just os
  x:xs -> case x of
    StkOpenParen -> Nothing
    StkUnaryPrefix _ f -> pusher (R.Unary f)
    StkBinary _ f -> pusher (R.Binary f)
    where
      pusher op = popRemainingOperators (xs, output') where
        output' = os |> (R.TokOperator op)
