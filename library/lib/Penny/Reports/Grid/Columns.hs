module Penny.Reports.Grid.Columns where

import qualified Penny.Core.Wells as Wells
import qualified Penny.Core.DateTime as DateTime
import qualified Penny.Core.Hours as Hours
import qualified Penny.Core.Minutes as Minutes
import qualified Penny.Core.Seconds as Seconds
import qualified Penny.Core.TimeZoneOffset as TimeZoneOffset
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Core.Number as Number
import qualified Penny.Core.Flag as Flag
import qualified Penny.Core.Payee as Payee
import qualified Penny.Core.Commodity as Commodity
import qualified Penny.Core.Tags as Tags
import qualified Penny.Core.Tag as Tag
import qualified Penny.Core.Account as Account
import qualified Penny.Core.SubAccount as SubAccount
import Rainbox
import Rainbow
import Data.Maybe
import Data.Monoid
import Data.Time
import qualified Data.Text as X
import qualified Data.Foldable as F
import Data.List (intersperse)

-- | Produces a cell with one line aligned on the left; uses the
-- default background colors, and uses the supplied foreground colors.
oneLineLeft
  :: (Wells.T -> X.Text)
  -- ^ Supplies the text of the cell
  -> Both
  -- ^ Foreground colors
  -> (Align Horiz, Background -> Wells.T -> [Bar])
oneLineLeft getTxt fg = (left, fn)
  where
    fn bg wells = [Bar $ [fromText (getTxt wells) <> fore fg <> back bg]]


-- | The YYYY-MM-DD date only.
date
  :: Both
  -- ^ Foreground colors
  -> (Align Horiz, Background -> Wells.T -> [Bar])
date = oneLineLeft fn
  where
    fn wells = X.pack (show dY) <> dash <> X.pack (show dM)
          <> dash <> X.pack (show dD)
      where
        (dY, dM, dD) = toGregorian . DateTime.day
          . Wells.dateTime $ wells
        dash = X.singleton '-'

-- | The HH:MM time, along with the seconds if the seconds are not
-- zero.
time
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
time = oneLineLeft $ \wells ->
  let (DateTime.T _ h m s _) = Wells.dateTime wells
      hm = (X.pack . show . Hours.toInt $ h)
        <> X.singleton ':' <> (X.pack . show . Minutes.toInt $ m)
      sec | Seconds.toInt s == 0 = X.empty
          | otherwise = X.singleton ':'
              <> (X.pack . show . Seconds.toInt $ s)
  in hm <> sec

-- | The time zone.
timeZone
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
timeZone = oneLineLeft $ \wells ->
  let (DateTime.T _ _ _ _ tz) = Wells.dateTime wells
      int = TimeZoneOffset.toInt tz
      pluMin | int < 0 = '-'
             | otherwise = '+'
  in X.singleton pluMin <> X.pack (show int)

clxn
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
clxn = oneLineLeft $ Clxn.toText . Wells.clxn

number
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
number = oneLineLeft
  $ fromMaybe X.empty
  . fmap Number.toText . Wells.number

flag
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
flag = oneLineLeft
  $ fromMaybe X.empty
  . fmap Flag.toText . Wells.flag

payee
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
payee = oneLineLeft
  $ fromMaybe X.empty
  . fmap Payee.toText . Wells.payee

-- Qty - how to handle? - show Trio if available, Qty otherwise?

commodity
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
commodity = oneLineLeft $ Commodity.toText . Wells.commodity

-- | Shows one Tag per line.
tags
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
tags fg = (left, fn)
  where
    fn bg = F.toList . fmap toBar . Tags.toSeq . Wells.tags
      where
        toBar tag = Bar . (:[])
          $ (fromText . Tag.toText $ tag) <> fore fg <> back bg

account
  :: Both
  -> (Align Horiz, Background -> Wells.T -> [Bar])
account = oneLineLeft
  $ X.concat . intersperse (X.singleton ':') . F.toList
  . fmap SubAccount.toText . Account.toSeq . Wells.account
