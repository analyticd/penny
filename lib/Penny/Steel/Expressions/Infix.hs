module Penny.Steel.Expressions.Infix
  ( InfixToken (..)
  , Paren(..)
  , createRPN
  ) where

import qualified Penny.Steel.Expressions.RPN as R
import qualified Data.Foldable as Fdbl

data InfixToken a
  = TokRPN (R.Token a)
  | TokParen Paren

data Paren = Open | Close

-- | Values on the operator stack.
data OpStackVal
  = StkOp R.Operator
  | StkOpenParen

-- In the shunting yard algorithm, the output sequence is a queue. The
-- first values to go into the output sequence are the first to be
-- processed by the RPN parser. In this module, the output sequence is
-- implemented as a list stack, which means it must be reversed upon
-- output (this is done in the createRPN function.)

processInfixToken
  :: ([OpStackVal], [R.Token a])
  -> InfixToken a
  -> Maybe ([OpStackVal], [R.Token a])
processInfixToken (os, ts) t = case t of
  TokRPN tok -> return $ processRPNToken (os, ts) tok
  TokParen p -> processParen (os, ts) p

processRPNToken
  :: ([OpStackVal], [R.Token a])
  -> R.Token a
  -> ([OpStackVal], [R.Token a])
processRPNToken (os, ts) t = case t of
  p@(R.TokOperand _ _) -> (os, p:ts)
  R.TokOperator d -> case d of
    R.OpNot -> (StkOp R.OpNot : os, ts)
    R.OpAnd -> (StkOp R.OpAnd : os, ts)
    R.OpOr ->
      let (os', ts') = popper os ts
      in (StkOp R.OpOr : os', ts')

popper :: [OpStackVal] -> [R.Token a] -> ([OpStackVal], [R.Token a])
popper os ts = case os of
  [] -> (os, ts)
  x:xs -> case x of
    StkOp R.OpAnd ->
      let os' = xs
          ts' = R.TokOperator R.OpAnd : ts
      in popper os' ts'
    _ -> (os, ts)

-- | Pops operators off the operator stack and onto the output stack
-- as long as the top of the operator stack is not an open
-- parenthesis. When an open parenthesis is encountered, pop that too,
-- but not onto the output stack. Fails if the stack has no open
-- parentheses.
popThroughOpen
  :: ([OpStackVal], [R.Token a])
  -> Maybe ([OpStackVal], [R.Token a])
popThroughOpen (os, ts) = case os of
  [] -> Nothing
  v:vs -> case v of
    StkOp op -> popThroughOpen (vs, R.TokOperator op : ts)
    StkOpenParen -> return (vs, ts)

processParen
  :: ([OpStackVal], [R.Token a])
  -> Paren
  -> Maybe ([OpStackVal], [R.Token a])
processParen (os, ts) p = case p of
  Open -> Just (StkOpenParen : os, ts)
  Close -> popThroughOpen (os, ts)

-- | Creates an RPN expression from an infix one. Fails only if there
-- are mismatched parentheses. It is possible to create a nonsensical
-- RPN expression; the RPN parser must catch this.
createRPN
  :: Fdbl.Foldable f
  => f (InfixToken a)
  -- ^ The input tokens, with the beginning of the expression on the
  -- left side of the sequence.

  -> Maybe [R.Token a]
  -- ^ The output sequence of tokens, with the beginning of the
  -- expression on the left side of the list.
createRPN ts = do
  (stack, toks) <- Fdbl.foldlM processInfixToken ([], []) ts
  fmap reverse $ popRemainingOperators stack toks

-- | Pops remaining items off operator stack. Fails if there is an
-- open paren left on the stack, as this indicates mismatched
-- parenthesis.
popRemainingOperators :: [OpStackVal] -> [R.Token a] -> Maybe [R.Token a]
popRemainingOperators os ts = case os of
  [] -> return ts
  x:xs -> case x of
    StkOp op -> popRemainingOperators xs (R.TokOperator op : ts)
    StkOpenParen -> Nothing
