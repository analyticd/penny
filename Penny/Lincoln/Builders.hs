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
-- a short custom Haskell program. Thus, this module. Many of the
-- functions here are partial--they simply apply "error" if you supply
-- invalid data, such as an sub-account name that is empty. Thus their
-- use is not recommended unless you are okay with your program
-- crashing nastily (which, face it, is just fine in many
-- circumstances). No function in this module or elsewhere in Penny
-- will create invalid data, because Penny never allows this.

module Penny.Lincoln.Builders where

import Control.Monad.Exception.Synchronous as Ex
import Control.Monad (when)
import qualified Data.List.Split as S
import qualified Data.List.NonEmpty as NE
import Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty (TextNonEmpty(TextNonEmpty))
import Data.Text (pack)
import qualified Data.Traversable as T

-- | Create an Account. You supply a single String, with colons to
-- separate the different sub-accounts. /This function is partial/. It
-- applies 'error' if the account name is not valid.
account :: String -> B.Account
account input = Ex.resolve error $ do
  subStrs <- case S.splitOn ":" input of
    []:[] -> Ex.throw "account name is null"
    (s:ss) -> return $ NE.nonEmpty s ss
  let makeSub s = case s of
        [] -> Ex.throw $
              "sub account name is null from account: " ++ input
        (c:cs) -> return $ B.SubAccountName (TextNonEmpty c (pack cs))
  subs <- T.traverse makeSub subStrs
  return $ B.Account subs
    
