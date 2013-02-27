module Penny.Steel.Expressions.Infix where

import qualified Penny.Steel.Expressions.RPN as R
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Fdbl

data InfixToken a
  = TokRPN (R.Token a)
  | TokParen Paren

data Paren = Open | Close

processInfixToken
  :: ([R.Operator], [R.Token a])
  -> InfixToken a
  -> Maybe ([R.Operator], [R.Token a])
processInfixToken (os, ts) t = case t of
  TokRPN tok -> return $ processRPNToken (os, t) tok
  TokParen p -> processParen (os, t) p

processRPNToken
  :: ([R.Operator], [R.Token a])
  -> R.Token a
  -> ([R.Operator], [R.Token a])
processRPNToken (os, ts) t = case t of
  p@(R.TokOperand _ _) -> (os, p:ts)
  R.TokOperator d -> case d of
    R.OpNot -> (R.OpNot : os, ts)
    R.OpAnd -> (R.OpAnd : os, ts)
    R.OpOr ->
      let (os', ts') = popper os ts
      in (R.OpOr : os', ts')

popper :: [R.Operator] -> [R.Token a] -> ([R.Operator], [R.Token a])
popper os ts = case os of
  [] -> (os, ts)
  x:xs -> case x of
    R.OpAnd -> let os' = xs
                   ts' = R.TokOperator R.OpAnd : ts
             in popper os' ts'
    _ -> (os, ts)

processParen
  :: ([R.Operator], [R.Token a])
  -> Paren
  -> Maybe ([R.Operator], [R.Token a])
processParen (os, ts) p = case p of
  
