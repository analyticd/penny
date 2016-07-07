{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
-- | The 'Core' forms a part of the 'Penny.Clatch.Clatch'.
module Penny.Core where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

import Penny.Serial
import Penny.Tranche (Postline, TopLine, Tranche)
import Penny.Troika

-- | The core of every posting.
data Core = Core
  { _troika :: Troika
  , _birth :: Serset
  -- ^ How this single posting relates to its sibling postings.
  -- Numbering restarts with every transaction.
  } deriving (Show, Generic)

instance PrettyVal Core

Lens.makeLenses ''Core

-- | A posting, coupled with metadata in the form of 'Tree' and a
-- 'Serpack' that indicates how this posting relates to other
-- postings.
--
-- The 'Posting' and the 'Transaction' have a similar shape, so they share
-- common functions.
type Posting a = (Serpack, (Postline a, Core))

-- | A list of postings, coupled with metadata in the form of 'Tree'
-- and with a 'Serpack' that indicates how this transaction relates to
-- other transactions.
--
-- The 'Posting' and the 'Transaction' have a similar shape, so they share
-- common functions.
type Transaction a = (Serpack, (TopLine a, Seq (Posting a)))

-- # Functions on postings and transactions

core :: Lens' (Posting a) Core
core = Lens._2 . Lens._2

postings :: Lens' (Transaction a) (Seq (Posting a))
postings = Lens._2 . Lens._2

-- |
-- @
-- 'serpack' :: 'Lens'' 'Transaction' 'Serpack'
-- 'serpack' :: 'Lens'' 'Posting' 'Serpack'
-- @
serpack :: Lens' (Serpack, a) Serpack
serpack = Lens._1

-- |
-- @
-- 'tranche' :: 'Lens'' ('Transaction' a) ('TopLine' a)
-- 'tranche' :: 'Lens'' ('Posting' a) ('Postline' a)
-- @
tranche :: forall a b c. Lens' (Serpack, (Tranche b a, c)) (Tranche b a)
tranche = Lens._2 . Lens._1

postline :: forall a c. Lens' (Serpack, (Postline a, c)) (Postline a)
postline = tranche

topLine :: forall a c. Lens' (Serpack, (TopLine a, c)) (TopLine a)
topLine = tranche
