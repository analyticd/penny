module Typist.Type2 where

import qualified Typist.Kind2 as K2
import qualified Typist.Constructor as Ctor
import qualified Typist.Typedesc as Typedesc
import qualified Typist.Typename as Ty
import qualified Typist.Identifier as Identifier

data T = T
  { name :: Identifier.T
  , ctors :: [Ctor.T K2.T]
  } deriving (Eq, Ord, Show)

toTypedesc :: T -> Ty.T -> Ty.T -> Typedesc.T
toTypedesc t t0 t1 = Typedesc.T (Ty.T (name t) [t0, t1]) cs
  where
    cs = fmap (fmap (K2.merge t0 t1)) . ctors $ t
