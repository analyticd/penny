module Typist.NoVariables where

import qualified Typist.Typedesc as Td
import qualified Typist.Typename as Ty
import qualified Typist.Constructor as Ctor

nullary
  :: String
  -- ^ Type name
  -> [Ctor.T Ty.T]
  -- ^ Each constructor
  -> Td.T
nullary n ts = Td.T (Ty.T n []) ts

-- | A newtype.
wrapper
  :: String
  -- ^ Type name
  -> Ty.T
  -- ^ Wrapped type
  -> Td.T
wrapper n t = nullary n [Ctor.T n [t]]

opaque
  :: String
  -- ^ Type name
  -> Td.T
opaque n = Td.opaque (Ty.T n [])
