-- | Postfix, or RPN, expression parsing.
--
-- An RPN expression consists of operands and operators; a token is
-- either an operand or an operator. For example, in the expression @5
-- 4 +@, the @5@ and the @4@ are operands; the @+@ is an operator;
-- each of these three is a token.
module Penny.Steel.Expressions.RPN (
  Operand(..),
  Operator(Unary, Binary),
  Token(TokOperand, TokOperator),
  process) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl

-- | An operand; for example, in the expression @5 4 +@, @5@ and @4@
-- are operands.
data Operand a loc = Operand
  { opOperand :: a
  , opLocation :: loc
  } deriving Show

newtype StackVal a loc = StackVal
  { stkOperand :: a
  , stkLocation :: Maybe loc
  } deriving Show

-- | Operators; for example, in the expression @5 4 +@, @+@ is an
-- operator. Because this is RPN, there is no operator precedence.
data Operator a =
  Unary (a -> a)
  -- ^ Unary operators take only one operand (for example, a factorial
  -- operator).

  | Binary (a -> a -> a)
    -- ^ Binary operators take two operands (for example, an addition
    -- operator).

data OperatorWithLoc a loc = OperatorWithLoc
  { otOperator :: Operator a
  , otLocation :: loc
  } deriving Show

instance Show (Operator a) where
  show (Unary _) = "<unary operator>"
  show (Binary _) = "<binary operator>"

-- | A token is either an operator or an operand.
data Token a =
  TokOperand (Operand a)
  | TokOperator (OperatorWithLoc a)
  deriving Show


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
  -> [StackVal a]
  -> Maybe ([StackVal a])
processOperator t ds = case t of
  (Unary f) -> case ds of
    [] -> Nothing
    (StackVal x _):xs -> return $ (StackVal (f x) Nothing) : xs
  (Binary f) -> case ds of
    [] -> Nothing
    (StackVal x _):dss -> case dss of
      (StackVal y _):dsss ->
        return $ (StackVal (f y x) Nothing) : dsss
      [] -> Nothing

-- | Adds an operand to the top of the stack.
processOperand ::
  Operand a
  -> [StackVal a]
  -> [StackVal a]
processOperand (Operand a loc) rs = (StackVal a (Just loc)) : rs

data Error a
  = InsufficientOperands (OperatorWithLoc a)
  | EmptyStack
  | FullStack [StackVal a]

-- | Processes the next token. Fails if the next token is an operator
-- and fails; otherwise, returns the new stack of operands.
processToken ::
  [StackVal a]
  -> Token a
  -> Ex.Exceptional (Error a) ([StackVal a])
processToken s tok = case tok of
  TokOperand d -> return (processOperand d s)
  TokOperator t@(OperatorWithLoc op _) ->
    Ex.fromMaybe (InsufficientOperands t)
    $ processOperator op s

-- | Processes an entire input sequence of RPN tokens.
process
  :: Fdbl.Foldable f
  => f (Token a)
  -- ^ A stack of tokens, with the end of the expression being at the
  -- tail of the list and the beginning being at the head.

  -> Ex.Exceptional (Error a) a
  -- ^ Fails if there is not exactly one operand remaining on the
  -- stack at the end of the parse, or if at any time there are
  -- insufficient operands on the stack to parse an
  -- operator. Otherwise, succeeds and returns the result.
process ls = do
  os <- Fdbl.foldlM processToken [] ls
  case os of
    [] -> Ex.throw EmptyStack
    (Operand x) : [] -> return x
    xs -> Ex.throw $ FullStack xs
