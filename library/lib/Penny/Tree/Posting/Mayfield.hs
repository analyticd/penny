module Penny.Tree.Posting.Mayfield where

import qualified Penny.Core.Memo as Memo
import qualified Penny.Core.Tags as Tags
import qualified Penny.Core.Location as Location
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Core.Trio as Trio
import qualified Penny.Core.Flag as Flag
import qualified Penny.Core.Number as Number
import qualified Penny.Core.Payee as Payee
import qualified Penny.Core.Posting as Posting
import qualified Penny.Core.Account as Account
import qualified Penny.Core.Tag as Tag
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Commodity as Commodity
import qualified Penny.Core.Orient as Orient
import qualified Penny.Tree.Flag as Tree.Flag
import qualified Penny.Tree.Number as Tree.Number
import qualified Penny.Tree.Payee.Posting as Tree.Payee
import qualified Penny.Tree.Account.Unquoted as Unquoted
import qualified Penny.Tree.Account.Quoted as Quoted
import qualified Penny.Tree.Tag as Tree.Tag
import qualified Penny.Tree.Side as Tree.Side
import qualified Penny.Tree.Commodity as Tree.Commodity
import qualified Penny.Tree.Ingot as Ingot
import qualified Penny.Tree.Posting.Item as Item
import qualified Penny.Tree.Posting.Error as Error
import Data.Sequence (Seq, (|>), ViewL(..))
import qualified Data.Sequence as S

data T = T
  { flag :: Maybe Flag.T
  , number :: Maybe Number.T
  , payee :: Maybe Payee.T
  , account :: Maybe Account.T
  , tags :: Seq Tag.T
  , side :: Maybe Side.T
  , commodity :: Maybe (Commodity.T, Int)
  , ingot :: Maybe (Ingot.T, Int)
  } deriving (Eq, Ord, Show)

empty :: T
empty = T Nothing Nothing Nothing Nothing S.empty Nothing Nothing Nothing

procItems
  :: Seq Item.T
  -> Either Error.T ( Memo.T
                      -> Location.T
                      -> Global.T
                      -> Local.T
                      -> Posting.T )
procItems sq = goItems 0 empty sq >>= finish

goItems
  :: Int
  -- ^ Index
  -> T
  -> Seq Item.T
  -> Either Error.T T
goItems ix mayfield sqn = case S.viewl sqn of
  EmptyL -> return mayfield
  x :< xs -> do
    mayfield' <- addItem mayfield (x, ix)
    goItems (succ ix) mayfield' xs


addItem :: T -> (Item.T, Int) -> Either Error.T T
addItem t (item, ix) = case item of
  Item.Flag fl -> case flag t of
    Nothing -> return t { flag = Just . Tree.Flag.toCore $ fl }
    Just _ -> Left Error.DuplicateFlag
  Item.Number nu -> case number t of
    Nothing -> return t { number = Just . Tree.Number.toCore $ nu }
    Just _ -> Left Error.DuplicateNumber
  Item.Payee pa -> case payee t of
    Nothing -> return t { payee = Just . Tree.Payee.toCore $ pa }
    Just _ -> Left Error.DuplicatePayee
  Item.Unquoted ac -> case account t of
    Nothing -> return t { account = Just . Unquoted.toCore $ ac }
    Just _ -> Left Error.DuplicateAccount
  Item.Quoted ac -> case account t of
    Nothing -> return t { account = Just . Quoted.toCore $ ac }
    Just _ -> Left Error.DuplicateAccount
  Item.Tag ta -> return t { tags = tags t |> Tree.Tag.toCore ta }
  Item.Side si -> case side t of
    Nothing -> return t { side = Just . Tree.Side.toCore $ si }
    Just _ -> Left Error.DuplicateSide
  Item.Commodity cy -> case commodity t of
    Nothing ->
      return t { commodity = ( Just (Tree.Commodity.toCore $ cy, ix)) }
    Just _ -> Left Error.DuplicateCommodity
  Item.Ingot ig -> case ingot t of
    Nothing -> return t { ingot = Just (ig, ix) }
    Just _ -> Left Error.DuplicateIngot

finish
  :: T
  -> Either Error.T ( Memo.T
                      -> Location.T
                      -> Global.T
                      -> Local.T
                      -> Posting.T )
finish t = do
  tri <- finishTrio (commodity t) (side t) (ingot t)
  ac <- maybe (Left Error.NoAccount) Right . account $ t
  return $ \memo loc global local ->
    Posting.T memo (number t) (flag t) (payee t) (Tags.T . tags $ t)
              ac loc global local tri

finishTrio
  :: Maybe (Commodity.T, Int)
  -> Maybe Side.T
  -> Maybe (Ingot.T, Int)
  -> Either Error.T Trio.T
finishTrio Nothing Nothing Nothing = return $ Trio.E

finishTrio Nothing Nothing (Just (ig, _)) =
  either (Left . Error.Ingot) Right $ Ingot.toTrio Nothing Nothing ig

finishTrio Nothing (Just si) Nothing =
  return $ Trio.S si

finishTrio Nothing (Just si) (Just (ig, _)) =
  either (Left . Error.Ingot) Right $ Ingot.toTrio Nothing (Just si) ig

finishTrio (Just (cy, _)) Nothing Nothing = return $ Trio.C cy

finishTrio (Just (cy, _)) (Just si) Nothing =
  return $ Trio.SC si cy

finishTrio (Just (cy, cyIx)) si (Just (ig, igIx)) =
  either (Left . Error.Ingot) Right
  $ Ingot.toTrio (Just (cy, orientation cyIx igIx)) si ig

orientation
  :: Int
  -- ^ Commodity index
  -> Int
  -- ^ Ingot index
  -> Orient.T
orientation cyIx igIx
  | cyIx > igIx = Orient.CommodityOnRight
  | otherwise = Orient.CommodityOnLeft
