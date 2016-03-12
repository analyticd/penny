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

import Penny.Arrangement
import qualified Penny.Commodity as Commodity
import Penny.Copper.Types
import Penny.DateTime
import Penny.Decimal
import Penny.Digit
import Penny.Natural
import Penny.Polar
import Penny.Realm
import qualified Penny.Scalar as Scalar
import qualified Penny.Tree as Tree
import qualified Penny.Trio as Trio

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
    h = digitToInt (_r'Time'0'Hours time)
    m = digitToInt (_r'Time'2'Minutes time)
    s = fromMaybe 0 getSecs
    getSecs = Lens.preview getter time
      where
        getter = r'Time'3'ColonSeconds'Maybe
          . Lens._Wrapped' . Lens._Just . r'ColonSeconds'1'Seconds
          . Lens.to digitToInt . Lens.to fromIntegral


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
    d3 = digitToInt . _r'ZoneHrsMins'1'D0'2 $ zone
    d2 = digitToInt . _r'ZoneHrsMins'2'D0'3 $ zone
    d1 = digitToInt . _r'ZoneHrsMins'3'D0'9 $ zone
    d0 = digitToInt . _r'ZoneHrsMins'4'D0'9 $ zone
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
        changeSign . naturalToInteger $ novDecsToPositive d1 ds
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
c'T_DebitCredit (T_DebitCredit dc ws) = do
  pole <- c'DebitCredit dc
  _ <- c'WhiteSeq ws
  return $ Trio.S pole

c'T_DebitCredit_Commodity
  :: T_DebitCredit_Commodity
  -> Locator Trio.Trio
c'T_DebitCredit_Commodity (T_DebitCredit_Commodity dc0 w1 cy2 w3) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  _ <- c'WhiteSeq w3
  return $ Trio.SC p cy

c'T_DebitCredit_NonNeutral
  :: T_DebitCredit_NonNeutral
  -> Locator Trio.Trio
c'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral dc0 w1 nn2 w3) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  advance (t'NonNeutral nn2)
  _ <- c'WhiteSeq w3
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
  dc0 w1 c2 w3 nn4 w5) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  cy <- c'Commodity c2
  isSpace <- c'WhiteSeq w3
  _ <- advance (t'NonNeutral nn4)
  _ <- c'WhiteSeq w5
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
  dc0 w1 nn2 w3 c4 w5) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  _ <- advance (t'NonNeutral nn2)
  isSpace <- c'WhiteSeq w3
  cy <- c'Commodity c4
  _ <- c'WhiteSeq w5
  let repAnyRadix = case nn2 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
      arrangement = Arrangement CommodityOnLeft isSpace
  return $ Trio.QC repAnyRadix cy arrangement

c'T_Commodity :: T_Commodity -> Locator Trio.Trio
c'T_Commodity (T_Commodity cy0 w1) = do
  cy <- c'Commodity cy0
  _ <- c'WhiteSeq w1
  return $ Trio.C cy

c'T_Commodity_Neutral :: T_Commodity_Neutral -> Locator Trio.Trio
c'T_Commodity_Neutral (T_Commodity_Neutral cy0 w1 n2 w3) = do
  cy <- c'Commodity cy0
  isSpace <- c'WhiteSeq w1
  advance (t'Neutral n2)
  _ <- c'WhiteSeq w3
  let nilAnyRadix = case n2 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.NC nilAnyRadix cy (Arrangement CommodityOnLeft isSpace)

c'T_Neutral_Commodity :: T_Neutral_Commodity -> Locator Trio.Trio
c'T_Neutral_Commodity (T_Neutral_Commodity n0 w1 cy2 w3) = do
  advance (t'Neutral n0)
  isSpace <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  _ <- c'WhiteSeq w3
  let nilAnyRadix = case n0 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.NC nilAnyRadix cy (Arrangement CommodityOnRight isSpace)

c'T_Commodity_NonNeutral :: T_Commodity_NonNeutral -> Locator Trio.Trio
c'T_Commodity_NonNeutral (T_Commodity_NonNeutral cy0 w1 n2 w3) = do
  cy <- c'Commodity cy0
  isSpace <- c'WhiteSeq w1
  advance (t'NonNeutral n2)
  _ <- c'WhiteSeq w3
  let brimScalarAnyRadix = case n2 of
        NonNeutralRadCom _ nilRadCom -> Left nilRadCom
        NonNeutralRadPer nilRadPer -> Right nilRadPer
  return $ Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnLeft isSpace)

c'T_NonNeutral_Commodity :: T_NonNeutral_Commodity -> Locator Trio.Trio
c'T_NonNeutral_Commodity (T_NonNeutral_Commodity n0 w1 cy2 w3) = do
  advance (t'NonNeutral n0)
  isSpace <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  _ <- c'WhiteSeq w3
  let brimScalarAnyRadix = case n0 of
        NonNeutralRadCom _ nilRadCom -> Left nilRadCom
        NonNeutralRadPer nilRadPer -> Right nilRadPer
  return $ Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnRight isSpace)

c'T_Neutral :: T_Neutral -> Locator Trio.Trio
c'T_Neutral (T_Neutral n0 w1) = do
  advance (t'Neutral n0)
  _ <- c'WhiteSeq w1
  let nilAnyRadix = case n0 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.UU nilAnyRadix

c'T_NonNeutral :: T_NonNeutral -> Locator Trio.Trio
c'T_NonNeutral (T_NonNeutral n0 w1) = do
  advance (t'NonNeutral n0)
  _ <- c'WhiteSeq w1
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

-- | Converts a 'Tree'.  Adds a child tree to the end of the list of
-- child trees indicating the position.
c'Tree :: Tree -> Locator Tree.Tree
c'Tree x = do
  pos <- positionTree
  tree <- getTree
  return $ addPositionTree pos tree
  where
    getTree = case x of
      TreeScalarFirst sc may ->
        f <$> c'Scalar sc <*> c'BracketedForest'Maybe may
        where
          f scalar forest = Tree.Tree User (Just scalar) forest
      TreeForestFirst bf sc ->
        f <$> c'BracketedForest bf <*> c'Scalar'Maybe sc
        where
          f forest mayScalar = Tree.Tree User mayScalar forest
    addPositionTree pos = Lens.over Tree.children (`Lens.snoc` pos)

c'BracketedForest'Maybe :: BracketedForest'Maybe -> Locator (Seq Tree.Tree)
c'BracketedForest'Maybe (BracketedForest'Maybe may) = case may of
  Nothing -> return Seq.empty
  Just bf -> c'BracketedForest bf

c'BracketedForest :: BracketedForest -> Locator (Seq Tree.Tree)
c'BracketedForest (BracketedForest os0 w1 f2 cs3 w4)
  = advance (t'OpenSquare os0)
  *> advance (t'White'Seq w1)
  *> c'Forest f2
  <* advance (t'CloseSquare cs3)
  <* advance (t'White'Seq w4)


c'Scalar'Maybe :: Scalar'Maybe -> Locator (Maybe Scalar.Scalar)
c'Scalar'Maybe (Scalar'Maybe may) = case may of
  Nothing -> return Nothing
  Just sca -> fmap Just (c'Scalar sca)

c'CommaTree :: CommaTree -> Locator Tree.Tree
c'CommaTree (CommaTree comma whites1 tree whites2)
  = advance (t'Comma comma)
  *> advance (t'White'Seq whites1)
  *> c'Tree tree
  <* advance (t'White'Seq whites2)

c'CommaTree'Seq :: CommaTree'Seq -> Locator (Seq Tree.Tree)
c'CommaTree'Seq (CommaTree'Seq sq) = traverse c'CommaTree sq

c'Forest :: Forest -> Locator (Seq Tree.Tree)
c'Forest (Forest t0 w1 ts2) = f <$> c'Tree t0 <* c'WhiteSeq w1
  <*> c'CommaTree'Seq ts2
  where
    f t ts = t `Lens.cons` ts

c'TopLine :: TopLine -> Locator (Seq Tree.Tree)
c'TopLine (TopLine forest) = c'Forest forest

c'Posting :: Posting -> Locator (Pos, Trio.Trio, Seq Tree.Tree)
c'Posting x = do
  pos <- locate
  case x of
    PostingTrioFirst trio bf -> do
      convTrio <- c'Trio trio
      ts <- c'BracketedForest'Maybe bf
      return (pos, convTrio, ts)
    PostingNoTrio bf -> do
      ts <- c'BracketedForest bf
      return (pos, Trio.E, ts)

c'SemiPosting
  :: SemiPosting
  -> Locator (Pos, Trio.Trio, Seq Tree.Tree)
c'SemiPosting (SemiPosting s0 w1 p2)
  = advance (t'Semicolon s0)
  *> advance (t'White'Seq w1)
  *> c'Posting p2

c'SemiPosting'Seq
  :: SemiPosting'Seq
  -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'SemiPosting'Seq (SemiPosting'Seq sq)
  = traverse c'SemiPosting sq

c'PostingList
  :: PostingList
  -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'PostingList (PostingList p0 ps1)
  = Lens.cons <$> c'Posting p0 <*> c'SemiPosting'Seq ps1

c'PostingList'Maybe
  :: PostingList'Maybe
  -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'PostingList'Maybe (PostingList'Maybe may) = case may of
  Nothing -> return Seq.empty
  Just pl -> c'PostingList pl

c'Postings
  :: Postings
  -> Locator (Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'Postings (Postings oc0 w1 pl2 cc3 w4)
  = advance (t'OpenCurly oc0)
  *> advance (t'White'Seq w1)
  *> c'PostingList'Maybe pl2
  <* advance (t'CloseCurly cc3)
  <* advance (t'White'Seq w4)

c'TopLine'Maybe :: TopLine'Maybe -> Locator (Seq Tree.Tree)
c'TopLine'Maybe = maybe (return Seq.empty) c'TopLine . coerce

c'Transaction
  :: Transaction
  -> Locator (Seq Tree.Tree, Seq (Pos, Trio.Trio, Seq Tree.Tree))
c'Transaction (Transaction tl pstgs)
  = (,)
  <$> c'TopLine'Maybe tl
  <*> c'Postings pstgs

data PriceParts = PriceParts
  { _pricePos :: Pos
  , _priceTime :: ZonedTime
  , _priceFrom :: Commodity.Commodity
  , _priceTo :: Commodity.Commodity
  , _priceExch :: Decimal
  }

c'PluMin :: Num a => PluMin -> Locator (a -> a)
c'PluMin x = do
  advance (t'PluMin x)
  return $ case x of
    PluMin'Plus _ -> id
    PluMin'Minus _ -> negate

c'PluMinFs :: Num a => PluMinFs -> Locator (a -> a)
c'PluMinFs (PluMinFs pm sq) = c'PluMin pm <* advance (t'White'Seq sq)

c'PluMinFs'Maybe :: Num a => PluMinFs'Maybe -> Locator (a -> a)
c'PluMinFs'Maybe = maybe (return id) c'PluMinFs . coerce

c'Exch :: Exch -> Locator Decimal
c'Exch x = case x of
  ExchNeutral neu ws -> do
    advance (t'Neutral neu)
    advance (t'White'Seq ws)
    return . fmap (const 0) . toDecZero $ neu
  ExchNonNeutral pm nn ws -> do
    changeSign <- c'PluMinFs'Maybe pm
    advance (t'NonNeutral nn)
    advance (t'White'Seq ws)
    return . fmap (changeSign . naturalToInteger) . toDecPositive $ nn

c'CyExch :: CyExch -> Locator (Commodity.Commodity, Decimal)
c'CyExch x = case x of
  CyExchCy cy ws ex -> (,)
    <$> c'Commodity cy
    <* advance (t'White'Seq ws)
    <*> c'Exch ex
  CyExchExch ex cy ws -> (\a b -> (b, a))
    <$> c'Exch ex
    <*> c'Commodity cy
    <* advance (t'White'Seq ws)

c'TimeWhites'Optional :: TimeWhites'Optional -> Locator TimeOfDay
c'TimeWhites'Optional x = case x of
  TimeWhitesYes t w -> c'TimeOfDay t <* advance (t'White'Seq w)
  TimeWhitesNo -> return Time.midnight

c'ZoneWhites'Optional :: ZoneWhites'Optional -> Locator Time.TimeZone
c'ZoneWhites'Optional (ZoneWhitesYes z ws)
  = Time.minutesToTimeZone <$> c'Zone z <* advance (t'White'Seq ws)
c'ZoneWhites'Optional ZoneWhitesNo = return Time.utc

c'Price :: Price -> Locator PriceParts
c'Price (Price a0 w1 d2 w3 tw4 zw5 c6 w7 e8)
  = f
  <$> locate
  <* advance (t'AtSign a0)
  <* advance (t'White'Seq w1)
  <*> c'Day d2
  <* advance (t'White'Seq w3)
  <*> c'TimeWhites'Optional tw4
  <*> c'ZoneWhites'Optional zw5
  <*> c'Commodity c6
  <* advance (t'White'Seq w7)
  <*> c'CyExch e8
  where
    f loc day tod zone from (to, exch) = PriceParts loc
      (Time.ZonedTime (Time.LocalTime day tod) zone)
      from to exch

type TxnParts = (Seq Tree.Tree, Seq (Pos, Trio.Trio, Seq Tree.Tree))

c'FileItem
  :: FileItem
  -> Locator (Either PriceParts TxnParts)
c'FileItem x = case x of
  FileItem'Price p -> Left <$> c'Price p
  FileItem'Transaction t -> Right <$> c'Transaction t

c'FileItem'Seq
  :: FileItem'Seq
  -> Locator (Seq (Either PriceParts TxnParts))
c'FileItem'Seq = traverse c'FileItem . coerce

c'WholeFile
  :: WholeFile
  -> Locator (Seq (Either PriceParts TxnParts))
c'WholeFile (WholeFile w0 i1) = advance (t'White'Seq w0)
  *> c'FileItem'Seq i1
