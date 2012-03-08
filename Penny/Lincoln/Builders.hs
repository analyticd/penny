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

module Penny.Lincoln.Builders where

import Control.Monad.Exception.Synchronous as Ex
import Control.Monad (when)
import qualified Data.List.Split as S
import qualified Data.List.NonEmpty as NE
import Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty (TextNonEmpty(TextNonEmpty))
import Data.Text (pack)
import qualified Data.Traversable as T

-- | Makes a function partial. Use if you don't want to bother dealing
-- with the Exceptional type.
crashy :: Show e => Ex.Exceptional e a -> a
crashy = Ex.resolve (error . show)

-- | Create an Account. You supply a single String, with colons to
-- separate the different sub-accounts.
account :: String -> Ex.Exceptional String B.Account
account input = do
  subStrs <- case S.splitOn ":" input of
    []:[] -> Ex.throw "account name is null"
    (s:ss) -> return $ NE.nonEmpty s ss
  let makeSub s = case s of
        [] -> Ex.throw $
              "sub account name is null from account: " ++ input
        (c:cs) -> return $ B.SubAccountName (TextNonEmpty c (pack cs))
  subs <- T.traverse makeSub subStrs
  return $ B.Account subs
    
