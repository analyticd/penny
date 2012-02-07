module Penny.Zinc.Expressions.Infix (
  Precedence(Precedence),
  
  Associativity(ALeft,
                ARight),
  
  Token(TokUnaryPostfix,
        TokUnaryPrefix,
        TokBinary,
        TokOpenParen,
        TokCloseParen),
  
  Infix(Infix),
  infixToRPN
  ) where

import qualified Penny.Zinc.Expressions.RPN as R

newtype Precedence = Precedence Int deriving (Show, Eq, Ord)

data Associativity = ALeft | ARight deriving Show

data Token a =
  TokOperand a
  | TokUnaryPostfix (a -> a)
  | TokUnaryPrefix Precedence (a -> a)
  | TokBinary Precedence Associativity (a -> a -> a)
  | TokOpenParen
  | TokCloseParen

newtype Infix a = Infix [Token a]
                  deriving Show

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

newtype Stack a = Stack [StackVal a] deriving Show

newtype Output a = Output [R.Token a] deriving Show

infixToRPN :: Infix a -> Maybe (R.RPN a)
infixToRPN i = processTokens i >>= return . outputToRPNInput

appendToOutput :: R.Token a -> Output a -> Output a
appendToOutput tok (Output ts) = Output (tok:ts)

outputToRPNInput :: Output a -> R.RPN a
outputToRPNInput (Output ls) = R.RPN (reverse ls)

popTokens ::
  (Precedence -> Bool)
  -> Stack a
  -> Output a
  -> (Stack a, Output a)
popTokens f (Stack ss) os = case ss of
  [] -> noChange
  x:xs -> case x of
      StkOpenParen -> (Stack ss, os)
      StkUnaryPrefix p g -> popper (R.Unary g) p
      StkBinary p g -> popper (R.Binary g) p
    where
      popper tok pr =
        if f pr
        then popTokens f (Stack xs) output'
        else noChange
          where
            output' = appendToOutput (R.TokOperator tok) os
  where
    noChange = (Stack ss, os)

processToken ::
  Token a
  -> Stack a
  -> Output a
  -> Maybe (Stack a, Output a)
processToken t (Stack ss) os = case t of
  TokOperand a ->
    Just (Stack ss, appendToOutput (R.TokOperand (R.Operand a)) os)
  TokUnaryPostfix f ->
    Just (Stack ss, appendToOutput (R.TokOperator (R.Unary f)) os)
  TokUnaryPrefix p f -> let
    outputVal = StkUnaryPrefix p f
    in Just (Stack (outputVal:ss), os)
  TokBinary p a f -> let
    stackVal = StkBinary p f
    ((Stack stack'), output') =
      case a of
        ALeft -> popTokens (>= p) (Stack ss) os
        ARight -> popTokens (> p) (Stack ss) os
    in Just (Stack (stackVal:stack'), output')
  TokOpenParen ->
    Just (Stack (StkOpenParen:ss), os)
  TokCloseParen -> popThroughOpenParen (Stack ss) os

processTokens ::
  Infix a
  -> Maybe (Output a)
processTokens i = processTokens' i (Stack []) (Output [])

processTokens' ::
  Infix a
  -> Stack a
  -> Output a
  -> Maybe (Output a)
processTokens' (Infix is) st os = case is of
  [] -> popRemainingOperators st os
  t:ts -> do
    (stack', output') <- processToken t st os
    processTokens' (Infix ts) stack' output'

popRemainingOperators ::
  Stack a
  -> Output a
  -> Maybe (Output a)
popRemainingOperators (Stack ss) os = case ss of
  [] -> Just os
  x:xs -> case x of
      StkOpenParen -> Nothing
      StkUnaryPrefix _ f -> pusher (R.Unary f)
      StkBinary _ f -> pusher (R.Binary f)
    where
      pusher op = popRemainingOperators (Stack xs) output' where
        output' = appendToOutput (R.TokOperator op) os

popThroughOpenParen ::
  Stack a
  -> Output a
  -> Maybe (Stack a, Output a)
popThroughOpenParen (Stack ss) os = case ss of
  [] -> Nothing
  x:xs -> let
    popper op = popThroughOpenParen stack' output' where
      output' = appendToOutput (R.TokOperator op) os
      stack' = Stack xs
    in case x of
      StkUnaryPrefix _ f -> popper (R.Unary f)        
      StkBinary _ f -> popper (R.Binary f)
      StkOpenParen -> Just (Stack xs, os)
