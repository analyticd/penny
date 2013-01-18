-- | Partial functions that make common types in Lincoln. Some data
-- types in Lincoln are deeply nested, with TextNonEmpty nested inside
-- of a newtype, nested inside of a NonEmptyList, nested inside
-- of... :) All the nesting ensures to the maximum extent possible
-- that the type system reflects the restrictions that exist on
-- Penny's data. For example, it would make no sense to have an empty
-- account (that is, an account with no sub-accounts) or a sub-account
-- whose name is an empty Text.
--
-- The disadvantage of the nesting is that building these data types
-- can be tedious if, for example, you want to build some data within
-- a short custom Haskell program. Thus, this module.

module Penny.Lincoln.Builders
  ( account
  ) where

import qualified Penny.Lincoln.Bits as B
import qualified Data.Text as X

-- | Create an Account. You supply a single Text, with colons to
-- separate the different sub-accounts.
account :: X.Text -> B.Account
account s =
  if X.null s
  then B.Account []
  else B.Account . map B.SubAccount . X.splitOn (X.singleton ':') $ s

