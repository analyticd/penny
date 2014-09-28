-- | Processes a list of "Penny.Tree.TopLine.Item"; if there are no
-- duplicates, returns a function that, when applied to some
-- additional fields, will return a TopLine.
module Penny.Tree.TopLine.Mayfield where

import Data.Time
import qualified Data.Foldable as Fdbl
import qualified Penny.Core.Hours as Hours
import qualified Penny.Core.Minutes as Minutes
import qualified Penny.Core.Seconds as Seconds
import qualified Penny.Core.TimeZoneOffset as TZO
import qualified Penny.Tree.TopLine.Error as Error
import qualified Penny.Tree.Flag as Tree.Flag
import qualified Penny.Core.Flag as Flag
import qualified Penny.Core.Number as Number
import qualified Penny.Tree.Number as Tree.Number
import Data.Sequence (Seq)
import qualified Penny.Tree.TopLine.Item as Item
import qualified Penny.Core.Memo as Memo
import qualified Penny.Core.Location as Location
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Tree.Time as Time
import qualified Penny.Core.Payee as Payee
import qualified Penny.Core.DateTime as DateTime
import qualified Penny.Core.TopLine as TopLine

data T = T
  { time :: Maybe (Hours.T, Minutes.T, Seconds.T, TZO.T)
  , flag :: Maybe Flag.T
  , number :: Maybe Number.T
  } deriving (Eq, Ord, Show)

toCore
  :: Seq Item.T
  -> Either Error.T ( Day
                      -> Maybe Payee.T
                      -> Memo.T
                      -> Location.T
                      -> Clxn.T
                      -> Global.T
                      -> Local.T
                      -> TopLine.T )
toCore sq = do
  t <- Fdbl.foldlM addItem (T Nothing Nothing Nothing) sq
  finish t

addItem :: T -> Item.T -> Either Error.T T
addItem mayfield (Item.Time t) = do
  ti <- either (Left . Error.BadTime) Right . Time.toCore $ t
  case time mayfield of
    Nothing -> return $ mayfield { time = Just ti }
    Just _ -> Left Error.DuplicateTime

addItem mayfield (Item.Flag flg) = case flag mayfield of
  Nothing -> return $ mayfield { flag = Just $ Tree.Flag.toCore flg }
  Just _ -> Left Error.DuplicateFlag

addItem mayfield (Item.Number num) = case number mayfield of
  Nothing -> return $ mayfield { number = Just $ Tree.Number.toCore num }
  Just _ -> Left Error.DuplicateNumber


finish :: T -> Either Error.T ( Day
                                -> Maybe Payee.T
                                -> Memo.T
                                -> Location.T
                                -> Clxn.T
                                -> Global.T
                                -> Local.T
                                -> TopLine.T )
finish t =
  let (hr, mi, sec, tzo) = case time t of
        Nothing -> (Hours.zero, Minutes.zero, Seconds.zero,
                    TZO.zero)
        Just (h, m, s, ti) -> (h, m, s, ti)
      r day pye memo loc clxn glbl locl = TopLine.T
        (DateTime.T day hr mi sec tzo)
        memo (number t) (flag t) pye loc clxn glbl locl
  in return r
