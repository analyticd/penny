-- | Parses reverse polish notation expressions. This module needs
-- much better error messages (right now it has none).
--
-- An RPN expression consists of operands and operators; a token is
-- either an operand or an operator. For example, in the expression @5
-- 4 +@, the @5@ and the @4@ are operands; the @+@ is an operator;
-- each of these three is a token.
module Penny.Liberty.Expressions.RPN (
  Operand(Operand),
  Operator(Unary, Binary),
  Token(TokOperand, TokOperator),
  RPN,
  process) where

import Penny.Liberty.Stack (
  Stack, push, empty, View(Empty, Top),
  view)
import qualified Penny.Liberty.Queue as Q

-- | An operand; for example, in the expression @5 4 +@, @5@ and @4@
-- are operands.
newtype Operand a = Operand a deriving Show

-- | Operators; for example, in the expression @5 4 +@, @+@ is an
-- operator. Because this is RPN, there is no operator precedence.
data Operator a =
  Unary (a -> a)
  -- ^ Unary operators take only one operand (for example, a factorial
  -- operator).

  | Binary (a -> a -> a)
    -- ^ Binary operators take two operands (for example, an addition
    -- operator).

instance Show (Operator a) where
  show (Unary _) = "<unary operator>"
  show (Binary _) = "<binary operator>"

-- | A token is either an operator or an operand.
data Token a =
  TokOperand (Operand a)
  | TokOperator (Operator a)
  deriving Show

-- | Represents an RPN expression ready to be parsed. The tokens must
-- be enqueued from left to right, so that the beginning of the
-- expression is at the front of the queue and the end of the
-- expression is at the back of the queue.
type RPN a = Q.Queue (Token a)

-- | The stack of operands. When parsing an RPN expression, operands
-- are removed from the incoming queue of tokens and are pushed onto
-- this stack to await further processing. Operators are never pushed
-- onto the stack; therefore the stack holds only operands.
type Operands a = Stack (Operand a)

-- | Given an operator, and the stack of operands, process the
-- operator. When parsing an RPN expression, encountering an operator
-- at the front of the queue of tokens to be processed means that the
-- correct number of tokens (one, for a unary operator, or two, for a
-- binary operator) must be popped off the stack of operands. The
-- operator is then applied to the operands. For a binary operator,
-- the binary function is applied first to the operand that was lowest
-- on the stack, and then to the operand that was higher up on the
-- stack. The result of the operator is then pushed onto the top of
-- the stack.
--
-- This function fails if there were insufficient operands on the
-- stack to process the operator. If successful, returns the new
-- stack, with the processed operands removed and the result of the
-- operator pushed onto the top of the stack.
processOperator ::
  Operator a
  -> Operands a
  -> Maybe (Operands a)
processOperator t ds = case t of
  (Unary f) -> case view ds of
    Empty -> Nothing
    Top xs (Operand x) -> return $ push (Operand (f x)) xs
  (Binary f) -> case view ds of
    Empty -> Nothing
    Top dss (Operand x) -> case view dss of
      Top dsss (Operand y) ->
        return $ push (Operand (f y x)) dsss
      Empty -> Nothing

-- | Adds an operand to the top of the stack.
processOperand ::
  Operand a
  -> Operands a
  -> Operands a
processOperand = push

-- | Processes the next token. Fails if the next token is an operator
-- and fails; otherwise, returns the new stack of operands.
processToken ::
  Token a
  -> Operands a
  -> Maybe (Operands a)
processToken tok s = case tok of
  TokOperand d -> return (processOperand d s)
  TokOperator t -> processOperator t s

-- | Process an RPN expression.
--
-- Fails if an operator fails to process
-- because there were insufficient operands on the stack; also fails
-- if there are operands left on the stack after the entire expression
-- has been parsed. Otherwise, succeeds with the result.
process :: RPN a -> Maybe a
process i = case popTokens i of
  Just os -> case view os of
    Top oss (Operand x) -> case view oss of 
      Empty -> Just x
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

popTokens :: RPN a
             -> Maybe (Operands a)
popTokens i = case popTokens' i empty of
  Nothing -> Nothing
  Just (is, s') -> case Q.view is of
    Q.Empty -> return s'
    _ -> Nothing

-- | Recursively removes tokens from the input queue of tokens to be
-- parsed. The base case is an empty input queue. In this case the
-- input queue and list of operands is returned unchanged. The
-- recursive case is an input queue with at least one token. In this
-- case the token is processed and popTokens' is called again with the
-- resulting input queue and operand stack.
popTokens' :: RPN a
             -> Operands a
             -> Maybe (RPN a, Operands a)
popTokens' ts s = case Q.view ts of
  Q.Empty -> return (ts, s)
  Q.Front xs x -> do
    s' <- processToken x s
    popTokens' xs s'

--
-- Testing
--

-- 19
{-
_input :: RPN Int
_input = RPN [ TokOperand (Operand 4)
             , TokOperand (Operand 5)
             , TokOperand (Operand 8)
             , TokOperator (Binary (*))
             , TokOperand (Operand 6)
             , TokOperator (Binary (-))
             , TokOperator (Binary (+))
             , TokOperand (Operand 2)
             , TokOperator (Binary div) ]
-}
