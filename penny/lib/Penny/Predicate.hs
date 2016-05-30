{-# LANGUAGE RankNTypes #-}
module Penny.Predicate where

{-

import Control.Lens (to, Getter)
import Data.Monoid.Cancellative (isPrefixOf, isSuffixOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)

import Penny.Account
import Penny.Clatch
import Penny.Shortcut

accountIs :: Account -> Getter (Sliced a) Bool
accountIs a = account . to (== a)

accountPrefix :: Seq Text -> Getter (Sliced a) Bool
accountPrefix a = account . to (a `isPrefixOf`)

accountSuffix :: Seq Text -> Getter (Sliced a) Bool
accountSuffix a = account . to (a `isSuffixOf`)

accountInfix :: Seq Text -> Getter (Sliced a) Bool
accountInfix a = account . to f
  where
    f target
      | Seq.length target < Seq.length a = False
      | beginning == a = True
      | otherwise = f end
      where
        (beginning, end) = Seq.splitAt (Seq.length a) target
-}
