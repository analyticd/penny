module Penny.Accounts where

import Penny.NestedMap
import Penny.Posting
import Data.Maybe ( fromMaybe )
import qualified Penny.Groups.AtLeast1 as A1
import Penny.Total
import qualified Data.Map as M
import Data.Monoid

newtype Accounts =
  Accounts { unAccounts :: NestedMap SubAccountName [Posting] }

prependPosting :: Posting -> Maybe [Posting] -> [Posting]
prependPosting p = maybe [p] (p:)

preservePostings :: Maybe [Posting] -> [Posting]
preservePostings mp = fromMaybe [] mp

addPostingToAccounts :: Posting -> Accounts -> Accounts
addPostingToAccounts p (Accounts m) = Accounts m' where
  m' = deepModifyLabel m fs
  a = unAccount . account $ p
  fs = case A1.rest a of
    [] -> [(A1.first a, prependPosting p)]
    ss -> let
      ff = (A1.first a, preservePostings)
      lf = (last ss, prependPosting p)
      mfs = map (\an -> (an, preservePostings)) (init ss)
      in ff: ( mfs ++ (lf:[]))

accounts :: [Posting] -> Accounts
accounts = foldr addPostingToAccounts (Accounts empty)

newtype AccountTotalsOneLevel =
  AccountTotalsOneLevel {
    unAccountTotalsOneLevel :: NestedMap SubAccountName Total }

accountTotalsOneLevel :: Accounts -> AccountTotalsOneLevel
accountTotalsOneLevel (Accounts m) = let
  m' = fmap toTotal m
  toTotal = foldl addAmount (Total M.empty)
  in AccountTotalsOneLevel m'

newtype AccountTotalsCumulative =
  AccountTotalsCumulative {
    unAccountTotalsCumulative :: NestedMap SubAccountName Total }

{-
accountTotalsCumulative ::
  AccountTotalsOneLevel
  -> AccountTotalsCumulative
accountTotalsCumulative (AccountTotalsOneLevel top) = let
  totalLevel (l, (NestedMap m)) =
    if M.null m
    then (l, M.empty)
    else mappend l
         . mconcat
         . map totalLevel
         . M.elems
         $ m
  in undefined
-}
