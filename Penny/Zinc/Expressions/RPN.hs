module Penny.Zinc.Expressions.RPN where

newtype Operand a = Operand a deriving Show

data Operator a =
  Unary (a -> a)
  | Binary (a -> a -> a)

instance Show (Operator a) where
  show (Unary _) = "<unary operator>"
  show (Binary _) = "<binary operator>"

data Token a =
  TokOperand (Operand a)
  | TokOperator (Operator a)
  deriving Show

newtype Input a = Input [Token a]
                  deriving Show

newtype Stack a = Stack [Operand a]
                  deriving Show

processOperator ::
  Operator a
  -> Stack a
  -> Maybe (Stack a)
processOperator t (Stack ds) = case t of
  (Unary f) -> case ds of
    [] -> Nothing
    ((Operand x):xs) -> return (Stack ( Operand (f x) : xs ))
  (Binary f) -> case ds of
    [] -> Nothing
    (x:[]) -> Nothing
    ((Operand x):(Operand y):xs) ->
      return (Stack(Operand (f y x) : xs))

processOperand ::
  Operand a
  -> Stack a
  -> Stack a
processOperand a (Stack as) = Stack (a:as)

processToken ::
  Token a
  -> Stack a
  -> Maybe (Stack a)
processToken tok s = case tok of
  TokOperand d -> return (processOperand d s)
  TokOperator t -> processOperator t s

process :: Input a -> Maybe a
process i = case popTokens i of
  Just (Stack ((Operand x):[])) -> Just x
  _ -> Nothing

popTokens :: Input a
             -> Maybe (Stack a)
popTokens i = case popTokens' i (Stack []) of
  Nothing -> Nothing
  (Just ((Input is), s')) -> case is of
    [] -> return s'
    _ -> Nothing

popTokens' :: Input a
             -> Stack a
             -> Maybe (Input a, Stack a)
popTokens' (Input ts) s = case ts of
  [] -> return (Input ts, s)
  (x:xs) -> do
    s' <- processToken x s
    popTokens' (Input xs) s'

--
-- Testing
--

-- 19
_input :: Input Int
_input = Input [ TokOperand (Operand 4)
               , TokOperand (Operand 5)
               , TokOperand (Operand 8)
               , TokOperator (Binary (*))
               , TokOperand (Operand 6)
               , TokOperator (Binary (-))
               , TokOperator (Binary (+))
               , TokOperand (Operand 2)
               , TokOperator (Binary div) ]
