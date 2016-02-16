{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.ConvertAst where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Control.Lens
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (Day, TimeOfDay(TimeOfDay), TimeZone(TimeZone))
import qualified Data.Sequence as Seq
import Pinchot (terminals)

import Penny.Arrangement
import qualified Penny.Commodity as Commodity
import Penny.DateTime
import Penny.Digit
import Penny.Grammar
import Penny.Natural
import Penny.Polar
import qualified Penny.Trio as Trio

data Pos = Pos
  { _line :: !Int
  , _column :: !Int
  } deriving (Eq, Ord, Show)

makeLenses ''Pos

newtype Converter a = Converter { runConverter :: State Pos a }
  deriving (Functor, Applicative, Monad)

locate :: Converter Pos
locate = Converter $ State.get

advanceOne :: Char -> Converter ()
advanceOne c
  | c == '\n' = Converter $ do
      line += 1
      column .= 1
  | c == '\t' = Converter $ do
      col <- use column
      column .= (col + 8 - ((col - 1) `mod` 8))
  | otherwise = Converter $ column += 1

advance :: Traversable f => f Char -> Converter ()
advance = mapM_ advanceOne

c'Day :: Date -> Converter Day
c'Day date = advance (terminals date) >> return (c'Day'Date date)

c'TimeOfDay :: Time -> Converter TimeOfDay
c'TimeOfDay time = advance (terminals time) >> return (TimeOfDay h m s)
  where
    h = digitToInt (_r'Time'0'Hours time)
    m = digitToInt (_r'Time'2'Minutes time)
    s = fromMaybe 0 getSecs
    getSecs
      = time
      ^? r'Time'3'ColonSeconds'Maybe
      . _Wrapped'
      . _Just
      . r'ColonSeconds'1'Seconds
      . to digitToInt
      . to fromIntegral

c'Zone :: ZoneHrsMins -> Converter TimeZone
c'Zone zone = advance (terminals zone) >> return (TimeZone mins False "")
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

c'QuotedString :: QuotedString -> Converter Text
c'QuotedString qs = advance (terminals qs) >> return x
  where
    x = X.pack . catMaybes . toList . fmap toChar
      . view _Wrapped'
      . _r'QuotedString'1'QuotedChar'Seq
      $ qs
    toChar (QuotedChar'NonEscapedChar (NonEscapedChar x)) = Just x
    toChar (QuotedChar'EscSeq (EscSeq _ pld)) = payloadToChar pld
    payloadToChar c = case c of
      EscPayload'Backslash _ -> Just '\\'
      EscPayload'Newline _ -> Just '\n'
      EscPayload'DoubleQuote _ -> Just '"'
      EscPayload'Gap _ -> Nothing

c'UnquotedString :: UnquotedString -> Converter Text
c'UnquotedString us = advance (terminals us) >> return x
  where
    x = X.pack . toList . terminals $ us

c'UnquotedCommodity :: UnquotedCommodity -> Converter Commodity.Commodity
c'UnquotedCommodity c
  = advance (terminals c)
  >> return (X.pack . toList . terminals $ c)

c'QuotedCommodity :: QuotedCommodity -> Converter Commodity.Commodity
c'QuotedCommodity = c'QuotedString . view _Wrapped'

c'Commodity :: Commodity -> Converter Commodity.Commodity
c'Commodity x = case x of
  Commodity'UnquotedCommodity u -> c'UnquotedCommodity u
  Commodity'QuotedCommodity q -> c'QuotedCommodity q

c'WholeAny :: WholeAny -> Converter Integer
c'WholeAny a = advance (terminals a) >> return x
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
c'WhiteSeq :: White'Seq -> Converter Bool
c'WhiteSeq ws@(White'Seq sq) = advance (terminals ws) >> return b
  where
    b = not . Seq.null $ sq

c'DebitCredit :: DebitCredit -> Converter Pole
c'DebitCredit dc = advance (terminals dc) >> return p
  where
    p = case dc of
      DebitCredit'Debit _ -> debit
      DebitCredit'Credit _ -> credit

c'T_DebitCredit :: T_DebitCredit -> Converter Trio.Trio
c'T_DebitCredit (T_DebitCredit dc ws) = do
  pole <- c'DebitCredit dc
  _ <- c'WhiteSeq ws
  return $ Trio.S pole

c'T_DebitCredit_Commodity
  :: T_DebitCredit_Commodity
  -> Converter Trio.Trio
c'T_DebitCredit_Commodity (T_DebitCredit_Commodity dc0 w1 cy2 w3) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  _ <- c'WhiteSeq w3
  return $ Trio.SC p cy

c'T_DebitCredit_NonNeutral
  :: T_DebitCredit_NonNeutral
  -> Converter Trio.Trio
c'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral dc0 w1 nn2 w3) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  advance (terminals nn2)
  _ <- c'WhiteSeq w3
  let repAnyRadix = case nn2 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
  return $ Trio.Q repAnyRadix

c'T_DebitCredit_Commodity_NonNeutral
  :: T_DebitCredit_Commodity_NonNeutral
  -> Converter Trio.Trio
c'T_DebitCredit_Commodity_NonNeutral (T_DebitCredit_Commodity_NonNeutral
  dc0 w1 c2 w3 nn4 w5) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  cy <- c'Commodity c2
  isSpace <- c'WhiteSeq w3
  _ <- advance (terminals nn4)
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
  -> Converter Trio.Trio
c'T_DebitCredit_NonNeutral_Commodity (T_DebitCredit_NonNeutral_Commodity
  dc0 w1 nn2 w3 c4 w5) = do
  p <- c'DebitCredit dc0
  _ <- c'WhiteSeq w1
  _ <- advance (terminals nn2)
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

c'T_Commodity :: T_Commodity -> Converter Trio.Trio
c'T_Commodity (T_Commodity cy0 w1) = do
  cy <- c'Commodity cy0
  _ <- c'WhiteSeq w1
  return $ Trio.C cy

c'T_Commodity_Neutral :: T_Commodity_Neutral -> Converter Trio.Trio
c'T_Commodity_Neutral (T_Commodity_Neutral cy0 w1 n2 w3) = do
  cy <- c'Commodity cy0
  isSpace <- c'WhiteSeq w1
  advance (terminals n2)
  _ <- c'WhiteSeq w3
  let nilAnyRadix = case n2 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.NC nilAnyRadix cy (Arrangement CommodityOnLeft isSpace)

c'T_Neutral_Commodity :: T_Neutral_Commodity -> Converter Trio.Trio
c'T_Neutral_Commodity (T_Neutral_Commodity n0 w1 cy2 w3) = do
  advance (terminals n0)
  isSpace <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  _ <- c'WhiteSeq w3
  let nilAnyRadix = case n0 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.NC nilAnyRadix cy (Arrangement CommodityOnRight isSpace)

c'T_Commodity_NonNeutral :: T_Commodity_NonNeutral -> Converter Trio.Trio
c'T_Commodity_NonNeutral (T_Commodity_NonNeutral cy0 w1 n2 w3) = do
  cy <- c'Commodity cy0
  isSpace <- c'WhiteSeq w1
  advance (terminals n2)
  _ <- c'WhiteSeq w3
  let brimScalarAnyRadix = case n2 of
        NonNeutralRadCom _ nilRadCom -> Left nilRadCom
        NonNeutralRadPer nilRadPer -> Right nilRadPer
  return $ Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnLeft isSpace)

c'T_NonNeutral_Commodity :: T_NonNeutral_Commodity -> Converter Trio.Trio
c'T_NonNeutral_Commodity (T_NonNeutral_Commodity n0 w1 cy2 w3) = do
  advance (terminals n0)
  isSpace <- c'WhiteSeq w1
  cy <- c'Commodity cy2
  _ <- c'WhiteSeq w3
  let brimScalarAnyRadix = case n0 of
        NonNeutralRadCom _ nilRadCom -> Left nilRadCom
        NonNeutralRadPer nilRadPer -> Right nilRadPer
  return $ Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnRight isSpace)

c'T_Neutral :: T_Neutral -> Converter Trio.Trio
c'T_Neutral (T_Neutral n0 w1) = do
  advance (terminals n0)
  _ <- c'WhiteSeq w1
  let nilAnyRadix = case n0 of
        NeuCom _ nilRadCom -> Left nilRadCom
        NeuPer nilRadPer -> Right nilRadPer
  return $ Trio.UU nilAnyRadix

c'T_NonNeutral :: T_NonNeutral -> Converter Trio.Trio
c'T_NonNeutral (T_NonNeutral n0 w1) = do
  advance (terminals n0)
  _ <- c'WhiteSeq w1
  let brimScalarAnyRadix = case n0 of
        NonNeutralRadCom _ brimRadCom -> Left brimRadCom
        NonNeutralRadPer brimRadPer -> Right brimRadPer
  return $ Trio.US brimScalarAnyRadix

c'Trio :: Trio -> Converter Trio.Trio
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
