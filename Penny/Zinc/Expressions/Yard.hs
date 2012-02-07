module Penny.Zinc.Expressions.Yard where

import qualified Penny.Zinc.Expressions.RPN as R

newtype Precedence = Precedence Int deriving (Show, Eq, Ord)

data Associativity = ALeft | ARight deriving Show

data Token a =
  TokUnaryPostfix (a -> a)
  | TokUnaryPrefix Precedence (a -> a)
  | TokBinary Precedence Associativity (a -> a -> a)
  | TokOpenParen
  | TokCloseParen

newtype Input a = Input [Token a]

instance Show (Token a) where
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

infixToRPN :: Input a -> Maybe (R.Input a)
infixToRPN i = processTokens i >>= return . outputToRPNInput

appendToOutput :: R.Token a -> Output a -> Output a
appendToOutput tok (Output ts) = Output (tok:ts)

outputToRPNInput :: Output a -> R.Input a
outputToRPNInput (Output ls) = R.Input (reverse ls)

popTokens ::
  (Precedence -> Bool)
  -> Stack a
  -> Output a
  -> (Stack a, Output a)
popTokens f (Stack ss) os = let
  noChange = (Stack ss, os) in
  case ss of
    [] -> noChange
    x:xs -> let
      popper tok pr = let
        output' = appendToOutput (R.TokOperator tok) os in
        if f pr
        then popTokens f (Stack xs) output'
        else noChange
      in case x of
        StkOpenParen -> (Stack ss, os)
        StkUnaryPrefix p g -> popper (R.Unary g) p
        StkBinary p g -> popper (R.Binary g) p

processToken ::
  Token a
  -> Stack a
  -> Output a
  -> Maybe (Stack a, Output a)
processToken t (Stack ss) os = case t of
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
  Input a
  -> Maybe (Output a)
processTokens i = processTokens' i (Stack []) (Output [])

processTokens' ::
  Input a
  -> Stack a
  -> Output a
  -> Maybe (Output a)
processTokens' (Input is) st os = case is of
  [] -> popRemainingOperators st os
  t:ts -> do
    (stack', output') <- processToken t st os
    processTokens' (Input ts) stack' output'

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
