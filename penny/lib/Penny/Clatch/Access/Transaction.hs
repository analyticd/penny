-- | Providing accessors (lenses and functions) to the 'Transaction'
-- component of a clatch-like type.
module Penny.Clatch.Access.Transaction where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import Data.Text (Text)
import qualified Data.Time as Time

import Penny.Clatch.Types
import Penny.Core
import qualified Penny.Tranche as Tranche

-- | Operates on the original 'Transaction'.
--
-- @
-- 'transaction' :: 'Lens'' ('Sliced' a)    'Transaction'
-- 'transaction' :: 'Lens'' ('Converted' a) 'Transaction'
-- 'transaction' :: 'Lens'' ('Prefilt' a)   'Transaction'
-- 'transaction' :: 'Lens'' ('Sorted' a)    'Transaction'
-- 'transaction' :: 'Lens'' ('Totaled' a)   'Transaction'
-- 'transaction' :: 'Lens'' 'Clatch'        'Transaction'
-- @

transaction :: Lens' (Transaction l, a) (Transaction l)
transaction = Lens._1

zonedTime :: Lens' (Sliced l a) Time.ZonedTime
zonedTime = transaction . topLine . Tranche.zonedTime

day :: Lens' (Sliced l a) Time.Day
day = transaction . topLine . Tranche.day

timeOfDay :: Lens' (Sliced l a) Time.TimeOfDay
timeOfDay = transaction . topLine . Tranche.timeOfDay

timeZone :: Lens' (Sliced l a) Time.TimeZone
timeZone = transaction . topLine . Tranche.timeZone

timeZoneMinutes :: Lens' (Sliced l a) Int
timeZoneMinutes = transaction . topLine . Tranche.timeZoneMinutes

payee :: Lens' (Sliced l a) (Maybe Text)
payee = transaction . topLine . Tranche.payee
