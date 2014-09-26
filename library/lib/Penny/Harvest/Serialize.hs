-- | Adds serial numbers to postings and top lines.  Uses a stateful
-- monadic computation from "Penny.Harvest.Serialize.State" to track
-- the forward and reverse serial numbers.  The primary function here
-- is 'Penny.Harvest.Serialize.Packages.fromLocatePackages', which
-- transforms a "Penny.Harvest.Locate.Packages" to a
-- "Penny.Harvest.Serialize.Packages".
module Penny.Harvest.Serialize where

