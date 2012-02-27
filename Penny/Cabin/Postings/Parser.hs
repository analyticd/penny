-- | The Postings command line parser.
module Penny.Cabin.Postings.Parser where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous
  (Exceptional(Success, Exception))
import Data.Text (Text, pack)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Combinator (longOneArg, option)
import System.Console.MultiArg.Option (makeLongOpt)
import System.Console.MultiArg.Prim (ParserE, throw, nextArg)

import qualified Penny.Cabin.Colors as CC
import Penny.Liberty.Error (Error)
import Penny.Liberty.Combinator (runUntilFailure, nextWordIs)
import qualified Penny.Liberty.Error as Er
import qualified Penny.Liberty.Expressions as Ex
import qualified Penny.Liberty.Filter as LF
import qualified Penny.Liberty.Operands as Od
import qualified Penny.Liberty.Operators as Oo
import qualified Penny.Liberty.Types as Ty
import qualified Penny.Cabin.Class as Cl
import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Postings.Fields as Fl
import qualified Penny.Cabin.Postings.Options as Op
import qualified Penny.Cabin.Postings.Schemes.DarkBackground as DB
import qualified Penny.Cabin.Postings.Schemes.LightBackground as LB

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PriceBox)


wrapLiberty ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
wrapLiberty dtz dt rad sep st =
  fromLibertyState st
  <$> LF.parseOption dtz dt rad sep (toLibertyState st)

wrapColor :: State -> ParserE Error State
wrapColor st = mkSt <$> color where
  mkSt co = st { colors = co }

wrapBackground :: State -> ParserE Error State
wrapBackground st = mkSt <$> background where
  mkSt b = st { scheme = b }

wrapWidth :: State -> ParserE Error State
wrapWidth st = mkSt <$> widthArg where
  mkSt w = st { width = const w }

showField :: State -> ParserE Error State
showField st = mkSt <$> fieldArg "show" where
  mkSt f = st { fields = fields' } where
    oldFields = fields st
    fields' = f True oldFields

hideField :: State -> ParserE Error State
hideField st = mkSt <$> fieldArg "hide" where
  mkSt f = st { fields = fields' } where
    oldFields = fields st
    fields' = f False oldFields

parseArg ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
parseArg dtz dt rad sp st =
  wrapLiberty dtz dt rad sp st
  <|> wrapColor st
  <|> wrapBackground st
  <|> wrapWidth st
  <|> showField st
  <|> hideField st

parseArgs ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
parseArgs dtz dt rad sp st =
  option st $ do
    rs <- runUntilFailure (parseArg dtz dt rad sp) st
    if null rs then return st else return (last rs)

parseCommand ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> Op.Options
  -> M.CaseSensitive
  -> (Text -> Exceptional Text (Text -> Bool))
  -> ParserE Error State
parseCommand dtz dt rad sp op cs fact = let
  st = newState op cs fact
  in do
    nextWordIs (pack "postings") <|> nextWordIs (pack "pos")
    parseArgs dtz dt rad sp st
  
data State =
  State { sensitive :: M.CaseSensitive
        , factory :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [Ex.Token (Ty.PostingInfo -> Bool)]
        , postFilter :: [Ty.PostingInfo] -> [Ty.PostingInfo]
        , colors :: CC.ColorPref
        , scheme :: (PC.DrCrColors, PC.BaseColors) 
        , width :: Maybe Cl.ScreenWidth -> Op.ReportWidth
        , fields :: Fl.Fields Bool }

newState ::
  Op.Options
  -> M.CaseSensitive
  -> (Text -> Exceptional Text (Text -> Bool))
  -> State
newState op s f =
  State { sensitive = s
        , factory = f
        , tokens = []
        , postFilter = id
        , colors = Op.colorPref op
        , scheme = (Op.drCrColors op, Op.baseColors op)
        , width = Op.width op
        , fields = Op.defaultFields }

toLibertyState :: State -> LF.State
toLibertyState st =
  LF.State { LF.sensitive = sensitive st
           , LF.factory = factory st
           , LF.tokens = tokens st
           , LF.postFilter = postFilter st }

fromLibertyState :: State -> LF.State -> State
fromLibertyState st lf =
  State { sensitive = LF.sensitive lf
        , factory = LF.factory lf
        , tokens = LF.tokens lf
        , postFilter = LF.postFilter lf
        , colors = colors st
        , scheme = scheme st 
        , width = width st
        , fields = fields st }

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
