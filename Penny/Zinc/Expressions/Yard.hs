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

appendToOutput :: R.Token a -> Output a -> Output a
appendToOutput tok (Output ts) = Output (tok:ts)
