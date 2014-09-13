module Typist.Type1 where

import qualified Typist.Kind1 as K1
import qualified Typist.Constructor as Ctor
import qualified Typist.Typedesc as Typedesc
import qualified Typist.Typename as Ty

data T = T
  { name :: String
  , ctors :: [Ctor.T K1.T]
  } deriving (Eq, Ord, Show)

toTypedesc :: T -> Ty.T -> Typedesc.T
toTypedesc t t0 = Typedesc.T (Ty.T (name t) [t0]) cs
  where
    cs = fmap (fmap (K1.merge t0)) . ctors $ t
