module Penny.Accounts where

import Penny.NestedMap
import Penny.Posting
import Data.Maybe ( fromMaybe )
import qualified Penny.Groups.AtLeast1 as A1
import Penny.Total
import qualified Data.Map as M
import Data.Monoid
import Penny.Transaction
import Penny.Groups.FamilyMember

newtype Accounts =
  Accounts { unAccounts :: NestedMap SubAccountName [FamilyMember Posting] }

prependPosting :: FamilyMember Posting -> Maybe [FamilyMember Posting] -> [FamilyMember Posting]
prependPosting p = maybe [p] (p:)

preservePostings :: Maybe [FamilyMember Posting] -> [FamilyMember Posting]
preservePostings = fromMaybe []

addPostingToAccounts :: FamilyMember Posting -> Accounts -> Accounts
addPostingToAccounts p (Accounts m) = Accounts m' where
  m' = relabel m fs
  a = unAccount . account . member $ p
  fs = case A1.rest a of
    [] -> [(A1.first a, prependPosting p)]
    ss -> let
      ff = (A1.first a, preservePostings)
      lf = (last ss, prependPosting p)
      mfs = map (\an -> (an, preservePostings)) (init ss)
      in ff: ( mfs ++ (lf:[]))

accounts :: [FamilyMember Posting] -> Accounts
accounts = foldr addPostingToAccounts (Accounts empty)

newtype AccountTotalsOneLevel =
  AccountTotalsOneLevel {
    unAccountTotalsOneLevel :: NestedMap SubAccountName Total }

accountTotalsOneLevel :: Accounts -> AccountTotalsOneLevel
accountTotalsOneLevel (Accounts m) = let
  m' = fmap (mconcat . map total . map member) m
  in AccountTotalsOneLevel m'

newtype AccountTotalsCumulative =
  AccountTotalsCumulative {
    unAccountTotalsCumulative :: NestedMap SubAccountName Total }

accountTotalsCumulative :: AccountTotalsOneLevel
                           -> AccountTotalsCumulative
accountTotalsCumulative (AccountTotalsOneLevel m) =
  AccountTotalsCumulative . snd . cumulativeTotal $ m
