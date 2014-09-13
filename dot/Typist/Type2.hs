module Typist.Type2 where

import qualified Typist.Kind2 as K2
import qualified Typist.Constructor as Ctor
import qualified Typist.Typedesc as Typedesc
import qualified Typist.Typename as Ty

data T = T
  { name :: String
  , ctors :: [Ctor.T K2.T]
  } deriving (Eq, Ord, Show)

toTypedesc :: Ty.T -> Ty.T -> T -> Typedesc.T
toTypedesc t0 t1 t = Typedesc.T (name t) [t0, t1] cs
  where
    cs = fmap (fmap (K2.merge t0 t1)) . ctors $ t
