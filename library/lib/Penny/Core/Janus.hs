module Penny.Core.Janus where

import qualified Penny.Core.Anna.RadCom as RadCom
import qualified Penny.Core.Anna.RadPer as RadPer

-- | Anything that may be parameterized on a
-- 'Penny.Core.Anna.RadCom.T' or on a 'Penny.Core.Anna.RadPer.T'.
data T a
  = Comma (a RadCom.T)
  | Period (a RadPer.T)

isEqual
  :: (a RadCom.T -> a RadCom.T -> Bool)
  -> (a RadPer.T -> a RadPer.T -> Bool)
  -> T a
  -> T a
  -> Bool
isEqual fc fp a b = case (a, b) of
  (Comma x, Comma y) -> fc x y
  (Period x, Period y) -> fp x y
  _ -> False

compare
  :: (a RadCom.T -> a RadCom.T -> Ordering)
  -> (a RadPer.T -> a RadPer.T -> Ordering)
  -> T a
  -> T a
  -> Ordering
compare fc fp a b = case a of
  Comma x -> case b of
    Comma y -> fc x y
    _ -> LT
  Period x -> case b of
    Period y -> fp x y
    _ -> GT

show
  :: (a RadCom.T -> String)
  -> (a RadPer.T -> String)
  -> T a
  -> String
show fc fp t = "Penny.Janus." ++ cname ++ " (" ++ str ++ ")"
  where
    (cname, str) = case t of
      Comma c -> ("Comma", fc c)
      Period p -> ("Period", fp p)
