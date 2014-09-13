module Typist.Kind1 where

import qualified Typist.Typename as Ty

data T
  = Var0
  | Con Ty.T
  deriving (Eq, Ord, Show)

merge :: Ty.T -> T -> Ty.T
merge t0 t = case t of
  Var0 -> t0
  Con ty -> ty
