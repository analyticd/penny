module Penny.Zinc.Expressions.Infix (
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

import qualified Penny.Zinc.Expressions.RPN as R
import Penny.Zinc.Expressions.Queues
  (Back, Front, front, View(Empty, (:<)), view, push,
   emptyFront)

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

type Stack a = Front (StackVal a)

newtype Output a = Output [R.Token a] deriving Show

infixToRPN :: Back (Token a) -> Maybe (R.RPN a)
infixToRPN i = processTokens (front i) >>= return . outputToRPNInput

appendToOutput :: R.Token a -> Output a -> Output a
appendToOutput tok (Output ts) = Output (tok:ts)

outputToRPNInput :: Output a -> R.RPN a
outputToRPNInput (Output ls) = R.RPN (reverse ls)

popTokens ::
  (Precedence -> Bool)
  -> Stack a
  -> Output a
  -> (Stack a, Output a)
popTokens f ss os = case view ss of
  Empty -> noChange
  x:<xs -> case x of
      StkOpenParen -> (ss, os)
      StkUnaryPrefix p g -> popper (R.Unary g) p
      StkBinary p g -> popper (R.Binary g) p
    where
      popper tok pr =
        if f pr
        then popTokens f xs output'
        else noChange
          where
            output' = appendToOutput (R.TokOperator tok) os
  where
    noChange = (ss, os)

processToken ::
  Token a
  -> Stack a
  -> Output a
  -> Maybe (Stack a, Output a)
processToken t ss os = case t of
  TokOperand a ->
    Just (ss, appendToOutput (R.TokOperand (R.Operand a)) os)
  TokUnaryPostfix f ->
    Just (ss, appendToOutput (R.TokOperator (R.Unary f)) os)
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

processTokens ::
  Front (Token a)
  -> Maybe (Output a)
processTokens i = processTokens' i emptyFront (Output [])

processTokens' ::
  Front (Token a)
  -> Stack a
  -> Output a
  -> Maybe (Output a)
processTokens' is st os = case view is of
  Empty -> popRemainingOperators st os
  t:<ts -> do
    (stack', output') <- processToken t st os
    processTokens' ts stack' output'

popRemainingOperators ::
  Stack a
  -> Output a
  -> Maybe (Output a)
popRemainingOperators s os = case view s of
  Empty -> Just os
  x:<xs -> case x of
      StkOpenParen -> Nothing
      StkUnaryPrefix _ f -> pusher (R.Unary f)
      StkBinary _ f -> pusher (R.Binary f)
    where
      pusher op = popRemainingOperators xs output' where
        output' = appendToOutput (R.TokOperator op) os

popThroughOpenParen ::
  Stack a
  -> Output a
  -> Maybe (Stack a, Output a)
popThroughOpenParen ss os = case view ss of
  Empty -> Nothing
  x:<xs -> let
    popper op = popThroughOpenParen xs output' where
      output' = appendToOutput (R.TokOperator op) os
    in case x of
      StkUnaryPrefix _ f -> popper (R.Unary f)        
      StkBinary _ f -> popper (R.Binary f)
      StkOpenParen -> Just (xs, os)
