module Typist.Type3 where

import qualified Typist.Kind3 as K3
import qualified Typist.Constructor as Ctor
import qualified Typist.Typedesc as Typedesc
import qualified Typist.Typename as Ty
import qualified Typist.Identifier as Identifier

data T = T
  { name :: Identifier.T
  , ctors :: [Ctor.T K3.T]
  } deriving (Eq, Ord, Show)

toTypedesc :: T -> Ty.T -> Ty.T -> Ty.T -> Typedesc.T
toTypedesc t t0 t1 t2 = Typedesc.T (Ty.T (name t) [t0, t1, t2]) cs
  where
    cs = fmap (fmap (K3.merge t0 t1 t2)) . ctors $ t
