{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Obtaining transactions and prices from a Copper-formatted file
-- takes three steps: parsing, locating, and proofing.  This
-- module performs locating.
--
-- Locating cannot fail; that is, any valid parse result will
-- successfully convert.
--
-- Locating takes the data types from "Penny.Copper.Types" and does
-- most of the work of converting them to types that are used in the
-- rest of the library.  More importantly, conversion also assigns
-- file locations (that is, line and column numbers) where
-- necessary.  Because the Earley parser does not have location
-- tracking, this module simply scans all characters and keeps track
-- of the position as it progresses through the characters.
module Penny.Copper.Locator where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import qualified Control.Lens as Lens
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (Day, TimeOfDay(TimeOfDay), ZonedTime)
import qualified Data.Time as Time
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pinchot (Loc)

import Penny.Arrangement
import qualified Penny.Commodity as Commodity
import qualified Penny.Copper.Conversions as Conv
import Penny.Copper.Types
import Penny.Copper.DateTime
import Penny.Decimal
import Penny.Polar
import qualified Penny.Positive as Pos
import Penny.Realm
import qualified Penny.Scalar as Scalar
import qualified Penny.Tree as Tree
import qualified Penny.Trio as Trio

{-
data Pos = Pos
  { _line :: !Int
  , _column :: !Int
  } deriving (Eq, Ord, Show)

Lens.makeLenses ''Pos

newtype Locator a = Locator (State Pos a)
  deriving (Functor, Applicative, Monad)

-- | Runs a 'Locator' computation.  Starts with an initial
-- position with a line of 1 and a column of 1.
runLocator :: Locator a -> a
runLocator (Locator k)
  = State.evalState k (Pos 1 1)


locate :: Locator Pos
locate = Locator $ State.get

advanceOne :: Char -> Locator ()
advanceOne c
  | c == '\n' = Locator $ do
      Lens.modifying line succ
      Lens.assign column 1
  | c == '\t' = Locator $ do
      col <- Lens.use column
      Lens.assign column (col + 8 - ((col - 1) `mod` 8))
  | otherwise = Locator $ Lens.modifying column succ

advance :: Traversable f => f Char -> Locator ()
advance = mapM_ advanceOne

c'Day :: Date -> Locator Day
c'Day date = advance (t'Date date) >> return (c'Day'Date date)


c'TimeOfDay :: Time -> Locator TimeOfDay
c'TimeOfDay time = advance (t'Time time) >> return (TimeOfDay h m s)
  where
    h = Conv.c'Int'Hours (_r'Time'0'Hours time)
    m = Conv.c'Int'Minutes (_r'Time'2'Minutes time)
    s = fromMaybe 0 getSecs
    getSecs = Lens.preview getter time
      where
        getter = r'Time'3'ColonSeconds'Maybe
          . Lens._Wrapped' . Lens._Just . r'ColonSeconds'1'Seconds
          . Lens.to Conv.c'Int'Seconds . Lens.to fromIntegral


c'Zone :: Zone -> Locator Int
c'Zone (Zone _ zone)
  = advance (t'ZoneHrsMins zone) >> return mins
  where
    mins
      = changeSign
      $ d3 * 10 ^ 3
      + d2 * 10 ^ 2
      + d1 * 10 * 1
      + d0
    d3 = Conv.c'Int'D0'2 . _r'ZoneHrsMins'1'D0'2 $ zone
    d2 = Conv.c'Int'D0'3 . _r'ZoneHrsMins'2'D0'3 $ zone
    d1 = Conv.c'Int'D0'9 . _r'ZoneHrsMins'3'D0'9 $ zone
    d0 = Conv.c'Int'D0'9 . _r'ZoneHrsMins'4'D0'9 $ zone
    changeSign = case _r'ZoneHrsMins'0'PluMin zone of
      PluMin'Plus _ -> id
      PluMin'Minus _ -> negate

c'QuotedString :: QuotedString -> Locator Text
c'QuotedString qs = advance (t'QuotedString qs) >> return x
  where
    x = X.pack . catMaybes . toList . fmap toChar
      . Lens.view Lens._Wrapped'
      . _r'QuotedString'1'QuotedChar'Seq
      $ qs
    toChar (QuotedChar'NonEscapedChar (NonEscapedChar x)) = Just x
    toChar (QuotedChar'EscSeq (EscSeq _ pld)) = payloadToChar pld
    payloadToChar c = case c of
      EscPayload'Backslash _ -> Just '\\'
      EscPayload'Newline _ -> Just '\n'
      EscPayload'DoubleQuote _ -> Just '"'
      EscPayload'Gap _ -> Nothing

c'UnquotedString :: UnquotedString -> Locator Text
c'UnquotedString us = advance (t'UnquotedString us) >> return x
  where
    x = X.pack . toList . t'UnquotedString $ us

c'UnquotedCommodity :: UnquotedCommodity -> Locator Commodity.Commodity
c'UnquotedCommodity c
  = advance (t'UnquotedCommodity c)
  >> return (X.pack . toList . t'UnquotedCommodity $ c)

c'QuotedCommodity :: QuotedCommodity -> Locator Commodity.Commodity
c'QuotedCommodity = c'QuotedString . Lens.view Lens._Wrapped'

c'Commodity :: Commodity -> Locator Commodity.Commodity
c'Commodity x = case x of
  Commodity'UnquotedCommodity u -> c'UnquotedCommodity u
  Commodity'QuotedCommodity q -> c'QuotedCommodity q

c'WholeAny :: WholeAny -> Locator Integer
c'WholeAny a = advance (t'WholeAny a) >> return x
  where
    x = case a of
      WholeAny'Zero _ -> 0
      WholeAny'WholeNonZero
        (WholeNonZero (PluMin'Maybe mayPm) d1 (D0'9'Seq ds)) ->
        changeSign . Pos.c'Integer'Positive $ Conv.novDecsToPositive d1 ds
        where
          changeSign = case mayPm of
            Nothing -> id
            Just (PluMin'Plus _) -> id
            Just (PluMin'Minus _) -> negate

-- | Returns True if there is at least one whitespace character.
c'WhiteSeq :: White'Seq -> Locator Bool
c'WhiteSeq ws@(White'Seq sq) = advance (t'White'Seq ws) >> return b
  where
    b = not . Seq.null $ sq


c'DebitCredit :: DebitCredit -> Locator Pole
c'DebitCredit dc = advance (t'DebitCredit dc) >> return p
  where
    p = case dc of
      DebitCredit'Debit _ -> debit
      DebitCredit'Credit _ -> credit

c'T_DebitCredit :: T_DebitCredit -> Locator Trio.Trio
c'T_DebitCredit (T_DebitCredit dc) = fmap Trio.S (c'DebitCredit dc)

c'T_DebitCredit_Commodity
  :: T_DebitCredit_Commodity
  -> Locator Trio.Trio
c'T_DebitCredit_Commodity (T_DebitCredit_Commodity dc0 w1 cy2) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  return $ Trio.SC p cy

c'T_DebitCredit_NonNeutral
  :: T_DebitCredit_NonNeutral
  -> Locator Trio.Trio
c'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral dc0 w1 nn2) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  advance (t'NonNeutral nn2)
  let repAnyRadix = case nn2 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
  return $ Trio.Q repAnyRadix

c'T_DebitCredit_Commodity_NonNeutral
  :: T_DebitCredit_Commodity_NonNeutral
  -> Locator Trio.Trio
c'T_DebitCredit_Commodity_NonNeutral (T_DebitCredit_Commodity_NonNeutral
  dc0 w1 c2 w3 nn4) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  cy <- c'Commodity c2
  isSpace <- c'WhiteSeq w3
  _ <- advance (t'NonNeutral nn4)
  let repAnyRadix = case nn4 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
      arrangement = Arrangement CommodityOnLeft isSpace
  return $ Trio.QC repAnyRadix cy arrangement

c'T_DebitCredit_NonNeutral_Commodity
  :: T_DebitCredit_NonNeutral_Commodity
  -> Locator Trio.Trio
c'T_DebitCredit_NonNeutral_Commodity (T_DebitCredit_NonNeutral_Commodity
  dc0 w1 nn2 w3 c4) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  _ <- advance (t'NonNeutral nn2)
  isSpace <- c'WhiteSeq w3
  cy <- c'Commodity c4
  let repAnyRadix = case nn2 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
      arrangement = Arrangement CommodityOnLeft isSpace
  return $ Trio.QC repAnyRadix cy arrangement

c'T_Commodity :: T_Commodity -> Locator Trio.Trio
c'T_Commodity (T_Commodity cy0) = do
  cy <- c'Commodity cy0
  return $ Trio.C cy

c'T_Commodity_Neutral :: T_Commodity_Neutral -> Locator Trio.Trio
c'T_Commodity_Neutral (T_Commodity_Neutral cy0 w1 n2) = do
  cy <- c'Commodity cy0
  isSpace <- c'WhiteSeq w1
  advance (t'Neutral n2)
  let nilAnyRadix = case n2 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.NC nilAnyRadix cy (Arrangement CommodityOnLeft isSpace)

c'T_Neutral_Commodity :: T_Neutral_Commodity -> Locator Trio.Trio
c'T_Neutral_Commodity (T_Neutral_Commodity n0 w1 cy2) = do
  advance (t'Neutral n0)
  isSpace <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  let nilAnyRadix = case n0 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.NC nilAnyRadix cy (Arrangement CommodityOnRight isSpace)

c'T_Commodity_NonNeutral :: T_Commodity_NonNeutral -> Locator Trio.Trio
c'T_Commodity_NonNeutral (T_Commodity_NonNeutral cy0 w1 n2) = do
  cy <- c'Commodity cy0
  isSpace <- c'WhiteSeq w1
  advance (t'NonNeutral n2)
  let brimScalarAnyRadix = case n2 of
        NonNeutralRadCom _ nilRadCom -> Left nilRadCom
        NonNeutralRadPer nilRadPer -> Right nilRadPer
  return $ Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnLeft isSpace)

c'T_NonNeutral_Commodity :: T_NonNeutral_Commodity -> Locator Trio.Trio
c'T_NonNeutral_Commodity (T_NonNeutral_Commodity n0 w1 cy2) = do
  advance (t'NonNeutral n0)
  isSpace <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  let brimScalarAnyRadix = case n0 of
        NonNeutralRadCom _ nilRadCom -> Left nilRadCom
        NonNeutralRadPer nilRadPer -> Right nilRadPer
  return $ Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnRight isSpace)

c'T_Neutral :: T_Neutral -> Locator Trio.Trio
c'T_Neutral (T_Neutral n0) = do
  advance (t'Neutral n0)
  let nilAnyRadix = case n0 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.UU nilAnyRadix

c'T_NonNeutral :: T_NonNeutral -> Locator Trio.Trio
c'T_NonNeutral (T_NonNeutral n0) = do
  advance (t'NonNeutral n0)
  let brimScalarAnyRadix = case n0 of
        NonNeutralRadCom _ brimRadCom -> Left brimRadCom
        NonNeutralRadPer brimRadPer -> Right brimRadPer
  return $ Trio.US brimScalarAnyRadix

c'Trio :: Trio -> Locator Trio.Trio
c'Trio x = case x of
  Trio'T_DebitCredit a -> c'T_DebitCredit a
  Trio'T_DebitCredit_Commodity a -> c'T_DebitCredit_Commodity a
  Trio'T_DebitCredit_NonNeutral a -> c'T_DebitCredit_NonNeutral a
  Trio'T_DebitCredit_Commodity_NonNeutral a ->
    c'T_DebitCredit_Commodity_NonNeutral a
  Trio'T_DebitCredit_NonNeutral_Commodity a ->
    c'T_DebitCredit_NonNeutral_Commodity a
  Trio'T_Commodity a -> c'T_Commodity a
  Trio'T_Commodity_Neutral a -> c'T_Commodity_Neutral a
  Trio'T_Neutral_Commodity a -> c'T_Neutral_Commodity a
  Trio'T_Commodity_NonNeutral a -> c'T_Commodity_NonNeutral a
  Trio'T_NonNeutral_Commodity a -> c'T_NonNeutral_Commodity a
  Trio'T_Neutral a -> c'T_Neutral a
  Trio'T_NonNeutral a -> c'T_NonNeutral a

c'Scalar :: Scalar -> Locator Scalar.Scalar
c'Scalar x = case x of
  Scalar'UnquotedString y -> Scalar.SText <$> c'UnquotedString y
  Scalar'QuotedString y -> Scalar.SText <$> c'QuotedString y
  Scalar'Date y -> Scalar.SDay <$> c'Day y
  Scalar'Time y -> Scalar.STime <$> c'TimeOfDay y
  Scalar'Zone y -> Scalar.SZone <$> c'Zone y
  Scalar'WholeAny y -> Scalar.SInteger <$> c'WholeAny y

positionTree :: Locator Tree.Tree
positionTree = fmap f locate
  where
    f pos = tree (Scalar.SText "position") [line, column]
      where
        tree scalar = Tree.Tree System (Just scalar)
        line = tree (Scalar.SText "line") [treeLine]
        column = tree (Scalar.SText "column") [treeCol]
        treeLine = tree (Scalar.SInteger (fromIntegral . _line $ pos)) []
        treeCol = tree (Scalar.SInteger (fromIntegral . _column $ pos)) []

c'WhitesScalar :: WhitesScalar -> Locator Scalar.Scalar
c'WhitesScalar (WhitesScalar ws sc)
  = c'WhiteSeq ws
  *> c'Scalar sc

c'WhitesScalar'Maybe :: WhitesScalar'Maybe -> Locator (Maybe Scalar.Scalar)
c'WhitesScalar'Maybe
  = maybe (return Nothing) (fmap Just . c'WhitesScalar)
  . coerce

c'Tree :: Tree -> Locator Tree.Tree
c'Tree t = do
  pos <- positionTree
  t' <- case t of
    Tree'ScalarMaybeForest a -> c'ScalarMaybeForest a
    Tree'ForestMaybeScalar a -> c'ForestMaybeScalar a
  return $ Lens.over Tree.children (`Lens.snoc` pos) t'

c'WhitesBracketedForest'Maybe
  :: WhitesBracketedForest'Maybe
  -> Locator (Seq Tree.Tree)
c'WhitesBracketedForest'Maybe
  = maybe (return Seq.empty) c'WhitesBracketedForest . coerce

c'WhitesBracketedForest
  :: WhitesBracketedForest
  -> Locator (Seq Tree.Tree)
c'WhitesBracketedForest (WhitesBracketedForest ws bf)
  = c'WhiteSeq ws
  *> c'BracketedForest bf

c'ForestMaybeScalar :: ForestMaybeScalar -> Locator Tree.Tree
c'ForestMaybeScalar (ForestMaybeScalar bf mayWS) = do
  frst <- c'BracketedForest bf
  maySC <- c'WhitesScalar'Maybe mayWS
  return $ Tree.Tree User maySC frst


c'ScalarMaybeForest :: ScalarMaybeForest -> Locator Tree.Tree
c'ScalarMaybeForest (ScalarMaybeForest sc mayWbf) = do
  sc' <- c'Scalar sc
  cs <- c'WhitesBracketedForest'Maybe mayWbf
  return $ Tree.Tree User (Just sc') cs

c'NextTree :: NextTree -> Locator Tree.Tree
c'NextTree (NextTree w0 c1 w2 t3)
  = advance (t'White'Seq w0)
  *> advance (t'Comma c1)
  *> advance (t'White'Seq w2)
  *> c'Tree t3

c'NextTree'Seq :: NextTree'Seq -> Locator (Seq Tree.Tree)
c'NextTree'Seq = traverse c'NextTree . coerce

c'Forest :: Forest -> Locator (Seq Tree.Tree)
c'Forest (Forest t1 ts) = Lens.cons <$> c'Tree t1 <*> c'NextTree'Seq ts

c'BracketedForest :: BracketedForest -> Locator (Seq Tree.Tree)
c'BracketedForest (BracketedForest os0 w1 f2 w2 cs3)
  = advance (t'OpenSquare os0)
  *> advance (t'White'Seq w1)
  *> c'Forest f2
  <* advance (t'White'Seq w2)
  <* advance (t'CloseSquare cs3)

c'TopLine :: TopLine -> Locator (Seq Tree.Tree)
c'TopLine = c'Forest . coerce

c'TrioMaybeForest :: TrioMaybeForest -> Locator (Trio.Trio, Seq Tree.Tree)
c'TrioMaybeForest (TrioMaybeForest tri may)
  = (,) <$> c'Trio tri <*> c'WhitesBracketedForest'Maybe may

c'Posting :: Posting -> Locator (Pos, Trio.Trio, Seq Tree.Tree)
c'Posting p = do
  pos <- locate
  case p of
    Posting'TrioMaybeForest a -> do
      (tri, ts) <- c'TrioMaybeForest a
      return (pos, tri, ts)
    Posting'BracketedForest bf -> do
      ts <- c'BracketedForest bf
      return (pos, Trio.E, ts)

c'NextPosting :: NextPosting -> Locator (Pos, Trio.Trio, Seq Tree.Tree)
c'NextPosting (NextPosting w0 s1 w2 p3)
  = advance (t'White'Seq w0)
  *> advance (t'Semicolon s1)
  *> advance (t'White'Seq w2)
  *> c'Posting p3

c'NextPosting'Seq
  :: NextPosting'Seq
  -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'NextPosting'Seq = traverse c'NextPosting . coerce

c'PostingList
  :: PostingList
  -> Locator ( (Pos, Trio.Trio, Seq Tree.Tree)
             , Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'PostingList (PostingList w0 p1 ps2)
  = (,)
  <$ advance (t'White'Seq w0)
  <*> c'Posting p1
  <*> c'NextPosting'Seq ps2

c'PostingList'Maybe
  :: PostingList'Maybe
  -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'PostingList'Maybe (PostingList'Maybe may) = case may of
  Nothing -> return Seq.empty
  Just pl -> fmap (uncurry Lens.cons) (c'PostingList pl)

c'Postings :: Postings -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'Postings (Postings o0 p1 w2 c3)
  = advance (t'OpenCurly o0)
  *> c'PostingList'Maybe p1
  <* advance (t'White'Seq w2)
  <* advance (t'CloseCurly c3)

c'Postings'Maybe
  :: Postings'Maybe
  -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'Postings'Maybe = maybe (return Seq.empty) c'Postings . coerce

c'TopLineMaybePostings
  :: TopLineMaybePostings
  -> Locator (Seq Tree.Tree, Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'TopLineMaybePostings (TopLineMaybePostings tl mayP)
  = (,)
  <$> c'TopLine tl
  <*> c'Postings'Maybe mayP

c'Transaction
  :: Transaction
  -> Locator (Seq Tree.Tree, Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'Transaction txn = case txn of
  Transaction'TopLineMaybePostings x -> c'TopLineMaybePostings x
  Transaction'Postings x -> do
    pstgs <- c'Postings x
    return (Seq.empty, pstgs)

c'PluMin :: Num a => PluMin -> Locator (a -> a)
c'PluMin x = do
  advance (t'PluMin x)
  return $ case x of
    PluMin'Plus _ -> id
    PluMin'Minus _ -> negate

c'PluMinNonNeutral
  :: Num a
  => PluMinNonNeutral
  -> Locator (a -> a, NonNeutral)
c'PluMinNonNeutral (PluMinNonNeutral pm ws nn)
  = (,)
  <$> c'PluMin pm
  <* advance (t'White'Seq ws)
  <* advance (t'NonNeutral nn)
  <*> pure nn

c'ExchNonNeu
  :: Num a
  => ExchNonNeu
  -> Locator (a -> a, NonNeutral)
c'ExchNonNeu x = case x of
  ExchNonNeu'PluMinNonNeutral a -> c'PluMinNonNeutral a
  ExchNonNeu'NonNeutral a ->
    (,) <$> pure id <* advance (t'NonNeutral a) <*> pure a

c'Exch :: Exch -> Locator Decimal
c'Exch x = case x of
  Exch'ExchNonNeu ne -> do
    (changeSign, nonNeu) <- c'ExchNonNeu ne
    return . fmap (changeSign . Pos.c'Integer'Positive)
      . c'DecPositive'NonNeutral $ nonNeu
  Exch'Neutral neu -> do
    advance (t'Neutral neu)
    return . fmap (const 0) . c'DecZero'Neutral $ neu

c'CyExch :: CyExch -> Locator (Commodity.Commodity, Decimal)
c'CyExch (CyExch c0 w1 e2) = do
  cy <- c'Commodity c0
  advance (t'White'Seq w1)
  d <- c'Exch e2
  return (cy, d)

c'ExchCy :: ExchCy -> Locator (Commodity.Commodity, Decimal)
c'ExchCy (ExchCy e0 w1 c2) = do
  d <- c'Exch e0
  advance (t'White'Seq w1)
  cy <- c'Commodity c2
  return (cy, d)

c'Janus :: Janus -> Locator (Commodity.Commodity, Decimal)
c'Janus x = case x of
  Janus'CyExch e -> c'CyExch e
  Janus'ExchCy e -> c'ExchCy e

c'WhitesTime :: WhitesTime -> Locator TimeOfDay
c'WhitesTime (WhitesTime w0 t1) = advance (t'White'Seq w0)
  *> c'TimeOfDay t1

c'WhitesZone :: WhitesZone -> Locator Time.TimeZone
c'WhitesZone (WhitesZone w0 z1)
  = Time.minutesToTimeZone
  <$ advance (t'White'Seq w0)
  <*> c'Zone z1

c'WhitesTime'Maybe :: WhitesTime'Maybe -> Locator TimeOfDay
c'WhitesTime'Maybe = maybe (return Time.midnight) c'WhitesTime . coerce

c'WhitesZone'Maybe :: WhitesZone'Maybe -> Locator Time.TimeZone
c'WhitesZone'Maybe = maybe (return Time.utc) c'WhitesZone . coerce

c'Price :: Price -> Locator PriceParts
c'Price (Price a0 w1 d2 wt3 wz4 w5 c6 w7 j8)
  = f
  <$> locate
  <* advance (t'AtSign a0)
  <* advance (t'White'Seq w1)
  <*> c'Day d2
  <*> c'WhitesTime'Maybe wt3
  <*> c'WhitesZone'Maybe wz4
  <* advance (t'White'Seq w5)
  <*> c'Commodity c6
  <* advance (t'White'Seq w7)
  <*> c'Janus j8
  where
    f loc day tod tz from (to, dec) = PriceParts loc zt from to dec
      where
        zt = Time.ZonedTime (Time.LocalTime day tod) tz

c'FileItem
  :: FileItem
  -> Locator (Either PriceParts TxnParts)
c'FileItem x = case x of
  FileItem'Price p -> Left <$> c'Price p
  FileItem'Transaction t -> Right <$> c'Transaction t

c'NextFileItem
  :: NextFileItem
  -> Locator (Either PriceParts TxnParts)
c'NextFileItem (NextFileItem w0 f1)
  = advance (t'White'Seq w0)
  *> c'FileItem f1

c'NextFileItem'Seq
  :: NextFileItem'Seq
  -> Locator (Seq (Either PriceParts TxnParts))
c'NextFileItem'Seq = traverse c'NextFileItem . coerce

-}

data PriceParts = PriceParts
  { _pricePos :: Loc
  , _priceTime :: ZonedTime
  , _priceFrom :: Commodity.Commodity
  , _priceTo :: Commodity.Commodity
  , _priceExch :: Decimal
  }

type TxnParts = (Seq Tree.Tree, Seq (Loc, Trio.Trio, Seq Tree.Tree))

c'WholeFile
  :: WholeFile Char Loc
  -> Seq (Either PriceParts TxnParts)
c'WholeFile = undefined
