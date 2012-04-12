module Penny.Cabin.Posts.Parser where

import Control.Applicative ((<|>), (<$>), (*>))
import Data.Text (Text, pack)
import System.Console.MultiArg.Combinator (longOneArg, option)
import System.Console.MultiArg.Option (makeLongOpt)
import System.Console.MultiArg.Prim (ParserE, throw)

import qualified Penny.Cabin.Chunk as CC
import Penny.Liberty.Error (Error)
import Penny.Liberty.Combinator (runUntilFailure, nextWordIs)
import qualified Penny.Liberty.Error as Er
import qualified Penny.Liberty.Filter as LF
import qualified Penny.Liberty.Operands as Od
import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Posts.Fields as Fl
import qualified Penny.Cabin.Posts.Numbered as Numbered
import qualified Penny.Cabin.Posts.Options as Op
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Shield as S

import Penny.Lincoln.Bits (DateTime)

type GetOpts = S.Runtime -> Op.T

wrapLiberty ::
  DateTime
  -> GetOpts
  -> S.Runtime
  -> ParserE Error (GetOpts)
wrapLiberty dt getOpts rt = let
  op = getOpts rt
  dtz = Op.timeZone op
  rg = Op.radGroup op
  dtz = Op.timeZone 

wrappedLiberty ::
  GetOpts
  -> ParserE Error (S.Runtime -> LF.State Numbered.T)
  
  
  
  f st' rt = let
    op = getOpts rt
    dtz = Op.timeZone op
    rg = Op.radGroup op
    st = toLibertyState op
    in fromLibertyState getOpts st'
  
  
  parseOpt rt = let
    op = getOpts rt
    dtz = Op.timeZone op
    rg = Op.radGroup op
    in LF.parseOption dtz dt rg (toLibertyState op)
  in fromLibertyState getOpts <$> parseOpt


wrapColor :: (GetOpts) -> ParserE Error (GetOpts)
wrapColor getOpts = mkSt <$> color where
  mkSt co rt = getOpts rt { Op.colorPref = co }

wrapBackground :: (GetOpts) -> ParserE Error (GetOpts)
wrapBackground getOpts = mkSt <$> background where
  mkSt b rt = getOpts rt { Op.drCrColors = fst b 
                         , Op.baseColors = snd b }

wrapWidth :: (GetOpts) -> ParserE Error (GetOpts)
wrapWidth getOpts = mkSt <$> widthArg where
  mkSt w rt = getOpts rt { Op.width = w }

showField :: (GetOpts) -> ParserE Error (GetOpts)
showField getOpts = mkSt <$> fieldArg "show" where
  mkSt f rt = getOpts rt { Op.fields = fields' } where
    oldFields = Op.fields (getOpts rt)
    fields' = f True oldFields

hideField :: (GetOpts) -> ParserE Error (GetOpts)
hideField getOpts = mkSt <$> fieldArg "hide" where
  mkSt f rt = getOpts rt  { Op.fields = fields' } where
    oldFields = Op.fields (getOpts rt)
    fields' = f False oldFields

parseArg ::
  DateTime
  -> (GetOpts)
  -> ParserE Error (GetOpts)
parseArg dt op =
  wrapLiberty dt op 
  <|> wrapColor op
  <|> wrapBackground op
  <|> wrapWidth op
  <|> showField op
  <|> hideField op

parseArgs ::
  DateTime
  -> (GetOpts)
  -> ParserE Error (GetOpts)
parseArgs dt op =
  option op $ do
    rs <- runUntilFailure (parseArg dt) op
    if null rs then return op else return (last rs)

parseCommand ::
  DateTime
  -> (GetOpts)
  -> ParserE Error (GetOpts)
parseCommand dt getOp =
    (nextWordIs (pack "postings")
     <|> nextWordIs (pack "pos"))
    *> parseArgs dt getOp
  
toLibertyState :: Op.T -> (LF.State Numbered.T)
toLibertyState op =
  LF.State { LF.sensitive = Op.sensitive op
           , LF.factory = Op.factory op
           , LF.tokens = Op.tokens op
           , LF.postFilter = Op.postFilter op }

fromLibertyState :: (GetOpts) -> LF.State Numbered.T -> (GetOpts)
fromLibertyState getOp lf = \s ->
  getOp s { Op.sensitive = LF.sensitive lf
          , Op.factory = LF.factory lf
          , Op.tokens = LF.tokens lf
          , Op.postFilter = LF.postFilter lf }

color :: ParserE Error (S.Runtime -> CC.Colors)
color = do
  let lo = makeLongOpt . pack $ "color"
  (_, t) <- longOneArg lo
  case pickColorArg t of
    Nothing -> throw $ Er.BadColorName t
    (Just col) -> return col
  
pickColorArg :: Text -> Maybe (S.Runtime -> CC.Colors)
pickColorArg t
  | t == pack "yes" = Just (const CC.Colors8)
  | t == pack "no" = Just (const CC.Colors0)
  | t == pack "256" = Just (const CC.Colors256)
  | t == pack "auto" = Just CO.maxCapableColors
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
