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
  
  Output,
  
  infixToRPN
  ) where

import qualified Penny.Liberty.Expressions.RPN as R
import Penny.Liberty.Queue
  (Queue, View(Empty, Front), view, enqueue, empty )
import Data.Stack (push)
import qualified Penny.Liberty.Stack as S

type Stack a = S.Stack (StackVal a)
type Output a = Queue (R.Token a)

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

popTokens ::
  (Precedence -> Bool)
  -> Stack a
  -> Output a
  -> (Stack a, Output a)
popTokens f ss os = case S.view ss of
  S.Empty -> noChange
  S.Top xs x -> case x of
    StkOpenParen -> (ss, os)
    StkUnaryPrefix p g -> popper (R.Unary g) p
    StkBinary p g -> popper (R.Binary g) p
    where
      popper tok pr =
        if f pr
        then popTokens f xs output'
        else noChange
          where
            output' = enqueue (R.TokOperator tok) os
  where
    noChange = (ss, os)

processToken ::
  Token a
  -> Stack a
  -> Output a
  -> Maybe (Stack a, Output a)
processToken t ss os = case t of
  TokOperand a ->
    Just (ss, enqueue (R.TokOperand (R.Operand a)) os)
  TokUnaryPostfix f ->
    Just (ss, enqueue (R.TokOperator (R.Unary f)) os)
  TokUnaryPrefix p f -> let
    outputVal = StkUnaryPrefix p f
    in Just (push outputVal ss, os)
  TokBinary p a f -> let
    stackVal = StkBinary p f
    (stack', output') =
      case a of
        ALeft -> popTokens (>= p) ss os
        ARight -> popTokens (> p) ss os
    in Just (push stackVal stack', output')
  TokOpenParen ->
    Just (push StkOpenParen ss, os)
  TokCloseParen -> popThroughOpenParen ss os

infixToRPN ::
  Queue (Token a)
  -> Maybe (Output a)
infixToRPN i = processTokens' i S.empty empty

processTokens' ::
  Queue (Token a)
  -> Stack a
  -> Output a
  -> Maybe (Output a)
processTokens' is st os = case view is of
  Empty -> popRemainingOperators st os
  Front ts t -> do
    (stack', output') <- processToken t st os
    processTokens' ts stack' output'

popRemainingOperators ::
  Stack a
  -> Output a
  -> Maybe (Output a)
popRemainingOperators s os = case S.view s of
  S.Empty -> Just os
  S.Top xs x -> case x of
    StkOpenParen -> Nothing
    StkUnaryPrefix _ f -> pusher (R.Unary f)
    StkBinary _ f -> pusher (R.Binary f)
    where
      pusher op = popRemainingOperators xs output' where
        output' = enqueue (R.TokOperator op) os

popThroughOpenParen ::
  Stack a
  -> Output a
  -> Maybe (Stack a, Output a)
popThroughOpenParen ss os = case S.view ss of
  S.Empty -> Nothing
  S.Top xs x -> let
    popper op = popThroughOpenParen xs output' where
      output' = enqueue (R.TokOperator op) os
    in case x of
      StkUnaryPrefix _ f -> popper (R.Unary f)        
      StkBinary _ f -> popper (R.Binary f)
      StkOpenParen -> Just (xs, os)
