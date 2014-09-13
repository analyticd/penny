module Typist.Kind3 where

import qualified Typist.Typename as Ty

data T
  = Var0
  | Var1
  | Var2
  | Con Ty.T
  deriving (Eq, Ord, Show)

merge :: Ty.T -> Ty.T -> Ty.T -> T -> Ty.T
merge t0 t1 t2 t = case t of
  Var0 -> t0
  Var1 -> t1
  Var2 -> t2
  Con ty -> ty
