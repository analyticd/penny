{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Types that group a posting with other interesting information.
module Penny.Clatch
  ( -- * Postings
    Core(..)
  , troika
  , birth

  , Posting
  , core
  , postings

  -- * Transactions
  , Transaction

  -- * Functions on postings and transactions
  , tranche
  , postline

  -- * Sersets
  , PreFiltset
  , Sortset
  , PostFiltset

  -- * Clatches and compatible types

  -- | These types are designed so that various functions and lenses
  -- can operate on values of multiple types.
  , Sliced
  , Converted
  , Prefilt
  , Sorted
  , Totaled
  , Clatch

  -- * Lenses and Functions on clatches and compatible types
  , transaction
  , slice
  , posting
  , converted
  , best
  , preFiltset
  , sortset
  , balance
  , postFiltset

  -- ** Field lenses
  , zonedTime
  , day
  , timeOfDay
  , timeZone
  , timeZoneMinutes
  , payee
  , number
  , flag
  , account
  , fitid
  , tags

  -- * Other field-related things
  , reconciled
  , cleared

  -- * Creation of clatches
  , addSerials
  , clatchesFromTransactions
  ) where

import Penny.Account
import Penny.Amount
import Penny.Balance
import Penny.Clatch.Types
import Penny.Clatch.Access.Balance
import Penny.Clatch.Access.Converted
import Penny.Clatch.Access.Posting
import Penny.Clatch.Access.PostFiltset
import Penny.Clatch.Access.PreFiltset
import Penny.Clatch.Access.Slice
import Penny.Clatch.Access.Sortset
import Penny.Clatch.Access.Transaction
import Penny.Clatch.Create
import Penny.Converter
import Penny.Core
import Penny.Copper.Decopperize
import Penny.Ents (Balanced, balancedToSeqEnt)
import qualified Penny.Fields as F
import Penny.SeqUtil
import Penny.Serial
import Penny.Tranche (Postline, TopLine, Tranche, fields)
import qualified Penny.Tranche as Tranche
import Penny.TransactionBare (TransactionBare(TransactionBare))
import Penny.Troika

import Control.Lens hiding (index)
import Control.Monad (join)
import Data.Bifunctor
import Data.Bifunctor.Flip
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Data.Traversable as T
import Data.Functor.Compose
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

-- # Lenses and functions on clatches

--
-- # Creation
--

-- # Lenses

