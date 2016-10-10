-- | Helpers with pretty printing.

module Penny.Pretty where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.OFX as OFX
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import qualified Data.Text as X
import Text.Show.Pretty (Value)
import qualified Text.Show.Pretty as Pretty

-- | Prettify a ZonedTime.
prettyZonedTime
  :: Time.ZonedTime
  -> Value
prettyZonedTime zt = Pretty.Con "ZonedTime" [Pretty.String . show $ zt]

-- | Prettify a Day.
prettyDay
  :: Time.Day
  -> Value
prettyDay day = Pretty.Con "Day" [Pretty.String . show $ day]

-- | Prettify a TimeOfDay.
prettyTimeOfDay
  :: Time.TimeOfDay
  -> Value
prettyTimeOfDay tod = Pretty.Con "TimeOfDay" [Pretty.String . show $ tod]

-- | Prettify a Text.
prettyText
  :: X.Text
  -> Value
prettyText x = Pretty.Con "Text" [Pretty.String . show $ x]

-- | Prettify a Maybe.
prettyMaybe
  :: (a -> Value)
  -> Maybe a
  -> Value
prettyMaybe _ Nothing = Pretty.Con "Nothing" []
prettyMaybe f (Just a) = Pretty.Con "Just" [f a]

-- | Prettify a TrnType.
prettyTrnType
  :: OFX.TrnType
  -> Value
prettyTrnType x = case Pretty.reify x of
  Nothing -> error "prettyTrnType failed"
  Just v -> v

-- | Prettify a Seq.
prettySeq
  :: (a -> Value)
  -> Seq.Seq a
  -> Value
prettySeq f = Pretty.Con "Seq" . (:[]) . Pretty.List . toList . fmap f

-- | Prettify a Map.
prettyMap
  :: (k -> Value)
  -> (v -> Value)
  -> Map.Map k v
  -> Value
prettyMap fk fv mp = Pretty.Con "Map"
  [ Pretty.List . map mkValue . Map.assocs $ mp ]
  where
    mkValue (k, v) = Pretty.Tuple [fk k, fv v]

prettyTuple2
  :: (a -> Value)
  -> (b -> Value)
  -> (a, b)
  -> Value
prettyTuple2 fa fb (a, b) = Pretty.Tuple [fa a, fb b]

prettyTuple3
  :: (a -> Value)
  -> (b -> Value)
  -> (c -> Value)
  -> (a, b, c)
  -> Value
prettyTuple3 fa fb fc (a, b, c) = Pretty.Tuple [fa a, fb b, fc c]

prettyUnit :: () -> Value
prettyUnit () = Pretty.Con "()" []
