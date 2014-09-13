module Typist.NoVariables where

import qualified Typist.Typedesc as Td
import qualified Typist.Typename as Ty
import qualified Typist.Constructor as Ctor
import qualified Typist.Identifier as Identifier

unit :: Identifier.T -> Td.T
unit idy = nullary idy [Ctor.T (Identifier.name idy) []]

nullary
  :: Identifier.T
  -- ^ Type name
  -> [Ctor.T]
  -- ^ Each constructor
  -> Td.T
nullary n ts = Td.T (Ty.T n []) ts

product
  :: Identifier.T
  -- ^ Type name
  -> [Ty.T]
  -- ^ Each contained type
  -> Td.T
product n = Td.T (Ty.T n []) . (:[])
  . Ctor.T (Identifier.name n)

-- | A newtype.
wrapper
  :: Identifier.T
  -- ^ Type name
  -> Ty.T
  -- ^ Wrapped type
  -> Td.T
wrapper n t = nullary n [Ctor.T (Identifier.name n) [t]]

abstract
  :: Identifier.T
  -- ^ Type name
  -> Td.T
abstract n = Td.abstract (Ty.T n [])
