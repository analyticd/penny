module Penny.Copper.Convert.Posting where

import Penny.Copper.Tree.Posting
import Data.Sequence
import qualified Data.Sequence as S
import Penny.Trio
import qualified Penny.Trio as E
import qualified Penny.Common as C
import qualified Penny.Posting as C
import qualified Penny.Numbers.Qty as C
import qualified Penny.Numbers.Abstract.Unsigned as C
import qualified Penny.Numbers.Abstract.Signed as C
import qualified Penny.Copper.Convert.Fields as F
import Penny.Numbers.Abstract.RadGroup
import qualified Penny.Serial as C

type MakePosting
  = C.Memo
  -> C.Location
  -> C.Serial
  -- ^ Global serial
  -> C.Serial
  -- ^ Collection serial
  -> C.Posting

convertToPosting
  :: Seq Item
  -> Either Error MakePosting
convertToPosting is = do
  acc <- scanItems is
  tri <- finalizeTrio acc
  account <- case scAccount acc of
    Nothing -> Left NoAccount
    Just a -> return a
  return $ \mem loc glbl clxn ->
    C.Posting { C.pstgMemo = mem
              , C.pstgNumber = scNumber acc
              , C.pstgFlag = scFlag acc
              , C.pstgPayee = scPayee acc
              , C.pstgTags = C.Tags $ scTags acc
              , C.pstgAccount = account
              , C.pstgLocation = loc
              , C.pstgGlobalSerial = glbl
              , C.pstgClxnSerial = clxn
              , C.pstgTrio = tri
              }

-- | Used to scan a sequence of Item.

data ScanAcc = ScanAcc
  { scFlag :: Maybe C.Flag
  , scNumber :: Maybe C.Number
  , scPayee :: Maybe C.Payee
  , scAccount :: Maybe C.Account
  , scTags :: Seq C.Tag
  , scDrCr :: Maybe C.Side

  , scCommodity :: Maybe (C.Commodity, Int)
  -- ^ The Int is the index where this item was found; to allow
  -- determination of whether the commodity is to the left or right of
  -- the quantity

  , scAmount
    :: Maybe (Either (F.AmountConv Period) (F.AmountConv Comma), Int)
  -- ^ The Int is the index where this item was found; to allow
  -- determination of whether the commodity is to the left or right of
  -- the quantity
  } deriving (Eq, Ord, Show)

emptyScanAcc :: ScanAcc
emptyScanAcc = ScanAcc Nothing Nothing Nothing
  Nothing S.empty Nothing Nothing Nothing

data Error
  = AlreadyFlag
  | AlreadyNumber
  | AlreadyPayee
  | AlreadyAccount
  | AlreadyDrCr
  | AlreadyCommodity
  | AlreadyAmount
  | DualCommodities
  | NumberIsZero
  | NoAccount
  deriving (Eq, Ord, Show)

scanItems
  :: Seq Item
  -> Either Error ScanAcc
scanItems= go 0 emptyScanAcc
  where
    go ix acc sq = case S.viewl sq of
      EmptyL -> Right acc
      i :< sq' -> case scanItem ix acc i of
        Left e -> Left e
        Right acc' -> go (succ ix) acc' sq'

scanItem :: Int -> ScanAcc -> Item -> Either Error ScanAcc
scanItem ix c i = case i of
  I0 fl -> maybe (Right (c { scFlag = Just . F.toFlag $ fl }))
    (const (Left AlreadyFlag)) (scFlag c)

  I1 nu -> maybe (Right (c { scNumber = Just . F.toNumber $ nu}))
    (const (Left AlreadyNumber)) (scNumber c)

  I2 pa -> maybe (Right (c { scPayee = Just . F.postingPayeeToPayee $ pa }))
    (const (Left AlreadyPayee)) (scPayee c)

  I3 ac -> maybe (Right (c { scAccount = Just
                             . F.unquotedAccountToAccount $ ac }))
    (const (Left AlreadyAccount)) (scAccount c)

  I4 ac -> maybe (Right (c { scAccount = Just
                             . F.quotedAccountToAccount $ ac }))
    (const (Left AlreadyAccount)) (scAccount c)

  I5 ta -> Right ( c { scTags = scTags c |> F.toTag ta })
  I6 dr -> maybe (Right (c { scDrCr = Just . F.debitToSide $ dr}))
    (const (Left AlreadyDrCr)) (scDrCr c)

  I7 cr -> maybe (Right (c { scDrCr = Just . F.creditToSide $ cr}))
    (const (Left AlreadyDrCr)) (scDrCr c)

  I8 cy -> maybe
    (Right (c { scCommodity = Just (F.toCommodity cy, ix)}))
    (const (Left AlreadyCommodity)) (scCommodity c)

  I9 cu -> maybe (Right (c { scCommodity = Just
                             ((F.currencyToCommodity cu), ix)}))
    (const (Left AlreadyCommodity)) (scCommodity c)

  I10 amp -> maybe (Right (c { scAmount = Just
    ((Left . F.amountPeriod $ amp), ix)}))
    (const (Left AlreadyAmount)) (scAmount c)

  I11 ac -> maybe (Right (c { scAmount = Just
    ((Right . F.amountComma $ ac), ix)}))
    (const (Left AlreadyAmount)) (scAmount c)


finalizeTrio :: ScanAcc -> Either Error Trio
finalizeTrio sa = do
  fa <- finalAmount sa
  finalAmountToTrio (scDrCr sa) fa

data FinalAmount
  = NoAmount
  | CommodityOnly C.Commodity
  | UnsignedOnly (Either (C.Unsigned Period) (C.Unsigned Comma))
  | UnsignedCy
      C.Arrangement
      (Either (C.Unsigned Period) (C.Unsigned Comma))
      C.Commodity

finalAmountToTrio
  :: Maybe C.Side
  -> FinalAmount
  -> Either Error E.Trio
finalAmountToTrio mayS fin = case fin of
  NoAmount -> case mayS of
    Nothing -> Right E.E
    Just s -> Right $ E.S s

  CommodityOnly c -> case mayS of
    Nothing -> Right $ E.C c
    Just s -> Right $ E.SC s c

  UnsignedOnly ei -> case mayS of
    Nothing -> fmap E.U $ eitherUnsignedToBrim ei
    Just s -> fmap E.Q $ eitherUnsignedToSigned s ei

  UnsignedCy ar ei cy -> case mayS of
    Nothing -> do
      brim <- eitherUnsignedToBrim ei
      return $ E.UC brim cy ar
    Just s -> do
      signed <- eitherUnsignedToSigned s ei
      return $ E.QC signed cy ar

eitherUnsignedToSigned
  :: C.Side
  -> Either (C.Unsigned a) (C.Unsigned b)
  -> Either Error (Either (C.Signed a C.Side) (C.Signed b C.Side))
eitherUnsignedToSigned s
  = either (fmap Left . unsignedToSigned s)
           (fmap Right . unsignedToSigned s)

unsignedToSigned
  :: C.Side
  -> C.Unsigned a
  -> Either Error (C.Signed a C.Side)
unsignedToSigned s = maybe (Left NumberIsZero) Right
  . C.signOffCenter s

eitherUnsignedToBrim
  :: Either (C.Unsigned Period) (C.Unsigned Comma)
  -> Either Error (Either (C.Brim Period) (C.Brim Comma))
eitherUnsignedToBrim ei = case ei of
  Left p -> fmap Left $ unsignedToBrim p
  Right c -> fmap Right $ unsignedToBrim c

unsignedToBrim
  :: C.Unsigned a
  -> Either Error (C.Brim a)
unsignedToBrim u = case u of
  C.Nil _ -> Left NumberIsZero
  C.Brim b -> Right b

finalAmount :: ScanAcc -> Either Error FinalAmount
finalAmount sc = case scCommodity sc of
  Nothing -> case scAmount sc of
    Nothing -> Right NoAmount
    Just (ei, _) -> Right $ case ei of
      Left (F.ACLeft pre) ->
        UnsignedCy (C.Arrangement C.CommodityOnLeft C.NoSpaceBetween)
                   (Left . F.preUnsigned $ pre)
                   (F.preCommodity pre)

      Left (F.ACRight post) -> case F.postCommodity post of
        Nothing -> UnsignedOnly (Left . F.postUnsigned $ post)
        Just cy -> UnsignedCy
          (C.Arrangement C.CommodityOnRight C.NoSpaceBetween)
          (Left . F.postUnsigned $ post) cy

      Right (F.ACLeft pre) ->
        UnsignedCy (C.Arrangement C.CommodityOnLeft C.NoSpaceBetween)
                   (Right . F.preUnsigned $ pre)
                   (F.preCommodity pre)

      Right (F.ACRight post) -> case F.postCommodity post of
        Nothing -> UnsignedOnly (Right . F.postUnsigned $ post)
        Just cy -> UnsignedCy
          (C.Arrangement C.CommodityOnRight C.NoSpaceBetween)
          (Right . F.postUnsigned $ post) cy

  Just (cy, cyIx) -> case scAmount sc of
    Nothing -> Right (CommodityOnly cy)
    Just (ei, amIx) -> case ei of
      Left (F.ACLeft _) -> Left DualCommodities
      Left (F.ACRight (F.ConvPostCurrency u mayC)) -> case mayC of
        Just _ -> Left DualCommodities
        Nothing -> Right
          $ UnsignedCy (arrangement cyIx amIx) (Left u) cy
      Right (F.ACRight (F.ConvPostCurrency u mayC)) -> case mayC of
        Just _ -> Left DualCommodities
        Nothing -> Right
          $ UnsignedCy (arrangement cyIx amIx) (Right u) cy
      Right (F.ACLeft _) -> Left DualCommodities

arrangement
  :: Int
  -- ^ Index of the commodity
  -> Int
  -- ^ Index of the amount
  -> C.Arrangement
arrangement c a = C.Arrangement o C.SpaceBetween
  where
    o | c > a = C.CommodityOnRight
      | otherwise = C.CommodityOnLeft

