module Penny.Cabin.Posts.Parser where

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
import qualified Penny.Cabin.Posts.Colors as PC
import qualified Penny.Cabin.Posts.Fields as Fl
import qualified Penny.Cabin.Posts.Options as Op
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Posts.Colors.DarkBackground as DB
import qualified Penny.Cabin.Posts.Colors.LightBackground as LB

import Penny.Lincoln.Bits (DateTime)

wrapLiberty ::
  DateTime
  -> Op.T Info.T
  -> ParserE Error (Op.T Info.T)
wrapLiberty dt op = let
  dtz = Op.timeZone op
  rg = Op.radGroup op
  in fromLibertyState op
     <$> LF.parseOption dtz dt rg (toLibertyState op)

wrapColor :: (Op.T a) -> ParserE Error (Op.T a)
wrapColor st = mkSt <$> color where
  mkSt co = st { Op.colorPref = co }

wrapBackground :: (Op.T a) -> ParserE Error (Op.T a)
wrapBackground st = mkSt <$> background where
  mkSt b = st { Op.drCrColors = fst b 
              , Op.baseColors = snd b }

wrapWidth :: (Op.T a) -> ParserE Error (Op.T a)
wrapWidth st = mkSt <$> widthArg where
  mkSt w = st { Op.width = w }

showField :: (Op.T a) -> ParserE Error (Op.T a)
showField st = mkSt <$> fieldArg "show" where
  mkSt f = st { Op.fields = fields' } where
    oldFields = Op.fields st
    fields' = f True oldFields

hideField :: (Op.T a) -> ParserE Error (Op.T a)
hideField st = mkSt <$> fieldArg "hide" where
  mkSt f = st { Op.fields = fields' } where
    oldFields = Op.fields st
    fields' = f False oldFields

parseArg ::
  DateTime
  -> (Op.T Info.T)
  -> ParserE Error (Op.T Info.T)
parseArg dt op =
  wrapLiberty dt op 
  <|> wrapColor op
  <|> wrapBackground op
  <|> wrapWidth op
  <|> showField op
  <|> hideField op

parseArgs ::
  DateTime
  -> (Op.T Info.T)
  -> ParserE Error (Op.T Info.T)
parseArgs dt op =
  option op $ do
    rs <- runUntilFailure (parseArg dt) op
    if null rs then return op else return (last rs)

parseCommand ::
  DateTime
  -> (Op.T Info.T)
  -> ParserE Error (Op.T Info.T)
parseCommand dt op =
    (nextWordIs (pack "postings")
     <|> nextWordIs (pack "pos"))
    *> parseArgs dt op
  
toLibertyState :: (Op.T Info.T) -> (LF.State Info.T)
toLibertyState op =
  LF.State { LF.sensitive = Op.sensitive op
           , LF.factory = Op.factory op
           , LF.tokens = Op.tokens op
           , LF.postFilter = Op.postFilter op }

fromLibertyState :: (Op.T Info.T) -> LF.State Info.T -> (Op.T Info.T)
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

pickField :: Text -> Maybe (a -> Fl.T a -> Fl.T a)
pickField t
  | t == pack "postingNum"       = Just Fl.t_postingNum
  | t == pack "visibleNum"       = Just Fl.t_visibleNum
  | t == pack "revPostingNum"    = Just Fl.t_revPostingNum
  | t == pack "lineNum"          = Just Fl.t_lineNum
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
  -> ParserE Error (Bool -> Fl.T Bool -> Fl.T Bool)
fieldArg str = do
  (_, t) <- longOneArg (makeLongOpt . pack $ str)
  case pickField t of
    Just fl -> return fl
    Nothing -> throw $ Er.BadFieldName t
