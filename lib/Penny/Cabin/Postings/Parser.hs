-- | The Postings command line parser.
module Penny.Cabin.Postings.Parser where

import Control.Applicative ((<|>), (<$>), (*>))
import Data.Text (Text, pack)
import System.Console.MultiArg.Combinator (longOneArg, option)
import System.Console.MultiArg.Option (makeLongOpt)
import System.Console.MultiArg.Prim (ParserE, throw)

import qualified Penny.Cabin.Colors as CC
import Penny.Liberty.Error (Error)
import Penny.Liberty.Combinator (runUntilFailure, nextWordIs)
import qualified Penny.Liberty.Error as Er
import qualified Penny.Liberty.Filter as LF
import qualified Penny.Liberty.Operands as Od
import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Postings.Fields as Fl
import qualified Penny.Cabin.Postings.Options as Op
import qualified Penny.Cabin.Postings.Types as LT
import qualified Penny.Cabin.Postings.Schemes.DarkBackground as DB
import qualified Penny.Cabin.Postings.Schemes.LightBackground as LB

import Penny.Lincoln.Bits (DateTime)


wrapLiberty ::
  DateTime
  -> Op.Options LT.PostingInfo
  -> ParserE Error (Op.Options LT.PostingInfo)
wrapLiberty dt op = let
  dtz = Op.timeZone op
  rg = Op.radGroup op
  in fromLibertyState op
     <$> LF.parseOption dtz dt rg (toLibertyState op)

wrapColor :: (Op.Options a) -> ParserE Error (Op.Options a)
wrapColor st = mkSt <$> color where
  mkSt co = st { Op.colorPref = co }

wrapBackground :: (Op.Options a) -> ParserE Error (Op.Options a)
wrapBackground st = mkSt <$> background where
  mkSt b = st { Op.drCrColors = fst b 
              , Op.baseColors = snd b }

wrapWidth :: (Op.Options a) -> ParserE Error (Op.Options a)
wrapWidth st = mkSt <$> widthArg where
  mkSt w = st { Op.width = w }

showField :: (Op.Options a) -> ParserE Error (Op.Options a)
showField st = mkSt <$> fieldArg "show" where
  mkSt f = st { Op.fields = fields' } where
    oldFields = Op.fields st
    fields' = f True oldFields

hideField :: (Op.Options a) -> ParserE Error (Op.Options a)
hideField st = mkSt <$> fieldArg "hide" where
  mkSt f = st { Op.fields = fields' } where
    oldFields = Op.fields st
    fields' = f False oldFields

parseArg ::
  DateTime
  -> (Op.Options LT.PostingInfo)
  -> ParserE Error (Op.Options LT.PostingInfo)
parseArg dt op =
  wrapLiberty dt op 
  <|> wrapColor op
  <|> wrapBackground op
  <|> wrapWidth op
  <|> showField op
  <|> hideField op

parseArgs ::
  DateTime
  -> (Op.Options LT.PostingInfo)
  -> ParserE Error (Op.Options LT.PostingInfo)
parseArgs dt op =
  option op $ do
    rs <- runUntilFailure (parseArg dt) op
    if null rs then return op else return (last rs)

parseCommand ::
  DateTime
  -> (Op.Options LT.PostingInfo)
  -> ParserE Error (Op.Options LT.PostingInfo)
parseCommand dt op =
    (nextWordIs (pack "postings")
     <|> nextWordIs (pack "pos"))
    *> parseArgs dt op
  
toLibertyState :: (Op.Options LT.PostingInfo) -> (LF.State LT.PostingInfo)
toLibertyState op =
  LF.State { LF.sensitive = Op.sensitive op
           , LF.factory = Op.factory op
           , LF.tokens = Op.tokens op
           , LF.postFilter = Op.postFilter op }

fromLibertyState :: (Op.Options LT.PostingInfo) -> LF.State LT.PostingInfo -> (Op.Options LT.PostingInfo)
fromLibertyState op lf =
  op  { Op.sensitive = LF.sensitive lf
      , Op.factory = LF.factory lf
      , Op.tokens = LF.tokens lf
      , Op.postFilter = LF.postFilter lf }

color :: ParserE Error CC.ColorPref
color = do
  let lo = makeLongOpt . pack $ "color"
  (_, t) <- longOneArg lo
  case pickColorArg t of
    Nothing -> throw $ Er.BadColorName t
    (Just col) -> return col
  
pickColorArg :: Text -> Maybe CC.ColorPref
pickColorArg t
  | t == pack "yes" = Just CC.Pref8
  | t == pack "no" = Just CC.Pref0
  | t == pack "256" = Just CC.Pref256
  | t == pack "auto" = Just CC.PrefAuto
  | otherwise = Nothing

background :: ParserE Error (PC.DrCrColors, PC.BaseColors)
background = do
  let lo = makeLongOpt . pack $ "background"
  (_, t) <- longOneArg lo
  case pickBackgroundArg t of
    Nothing -> throw $ Er.BadBackgroundArg t
    Just back -> return back

pickBackgroundArg :: Text -> Maybe (PC.DrCrColors, PC.BaseColors)
pickBackgroundArg t
  | t == pack "light" = Just (LB.drCrColors, LB.baseColors)
  | t == pack "dark" = Just (DB.drCrColors, DB.baseColors)
  | otherwise = Nothing

widthArg :: ParserE Error Op.ReportWidth
widthArg = do
  (_, t) <- longOneArg (makeLongOpt . pack $ "width")
  n <- Od.throwIf . Od.parseInt $ t
  return . Op.ReportWidth $ n

pickField :: Text -> Maybe (a -> Fl.Fields a -> Fl.Fields a)
pickField t
  | t == pack "linenum"          = Just Fl.t_lineNum
  | t == pack "date"             = Just Fl.t_date
  | t == pack "flag"             = Just Fl.t_flag
  | t == pack "number"           = Just Fl.t_number
  | t == pack "payee"            = Just Fl.t_payee
  | t == pack "account"          = Just Fl.t_account
  | t == pack "postingDrCr"      = Just Fl.t_postingDrCr
  | t == pack "postingCommodity" = Just Fl.t_postingCmdty
  | t == pack "postingQty"       = Just Fl.t_postingQty
  | t == pack "totalDrCr"        = Just Fl.t_totalDrCr
  | t == pack "totalCommodity"   = Just Fl.t_totalCmdty
  | t == pack "totalQty"         = Just Fl.t_totalQty
  | t == pack "tags"             = Just Fl.t_tags
  | t == pack "memo"             = Just Fl.t_memo
  | t == pack "filename"         = Just Fl.t_filename
  | otherwise                    = Nothing

fieldArg ::
  String
  -> ParserE Error (Bool -> Fl.Fields Bool -> Fl.Fields Bool)
fieldArg str = do
  (_, t) <- longOneArg (makeLongOpt . pack $ str)
  case pickField t of
    Just fl -> return fl
    Nothing -> throw $ Er.BadFieldName t
