module Penny.Cabin.Posts.Parser (parseCommand) where

import Control.Applicative ((<|>), (<$>), (*>), pure, (<$), many,
                            (<*>))
import Control.Monad ((>=>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toLower)
import qualified Data.Foldable as F
import Data.Text (Text, pack)
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Posts.Fields as Fl
import qualified Penny.Cabin.Posts.Options as Op
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Options as CO
import qualified Penny.Liberty as Ly
import qualified Penny.Shield as S

import Penny.Lincoln.Bits (DateTime)

data Error = BadColorName String
             | BadBackgroundArg String
             | BadWidthArg String
             | NoMatchingFieldName
             | MultipleMatchingFieldNames [String]
             deriving Show

-- | Parses the command line from the first word remaining up until,
-- but not including, the first non-option argment.
parseCommand ::
  S.Runtime
  -> Op.T
  -- ^ Default options for the posts report
  -> Parser (Ex.Exceptional Error Op.T)
parseCommand rt op = fmap f (many (parseOption rt))
  where
    folder = foldl (>=>) return
    f ls = (folder ls) op


parseOption ::
  S.Runtime
  -> Parser (Op.T -> Ex.Exceptional Error Op.T)
parseOption = undefined

parseOpt :: [String] -> [Char] -> C.ArgSpec a -> Parser a
parseOpt ss cs a = C.parseOption [C.OptSpec ss cs a]

color :: S.Runtime -> Parser (Op.T -> Ex.Exceptional Error Op.T)
color rt = parseOpt ["color"] "" (C.OneArg f)
  where
    f a1 op = case pickColorArg rt a1 of
      Nothing -> Ex.throw . BadColorName $ a1
      Just c -> return (op { Op.colorPref = c })

pickColorArg :: S.Runtime -> String -> Maybe CC.Colors
pickColorArg rt t
  | t == "yes" = Just CC.Colors8
  | t == "no" = Just CC.Colors0
  | t == "256" = Just CC.Colors256
  | t == "auto" = Just . CO.maxCapableColors $ rt
  | otherwise = Nothing

pickBackgroundArg :: String -> Maybe (PC.DrCrColors, PC.BaseColors)
pickBackgroundArg t
  | t == "light" = Just (LB.drCrColors, LB.baseColors)
  | t == "dark" = Just (DB.drCrColors, DB.baseColors)
  | otherwise = Nothing


background :: Parser (Op.T -> Ex.Exceptional Error Op.T)
background = parseOpt ["background"] "" (C.OneArg f)
  where
    f a1 op = case pickBackgroundArg a1 of
      Nothing -> Ex.throw . BadBackgroundArg $ a1
      Just (dc, bc) -> return (op { Op.drCrColors = dc
                                  , Op.baseColors = bc } )


width :: Parser (Op.T -> Ex.Exceptional Error Op.T)
width = parseOpt ["width"] "" (C.OneArg f)
  where
    f a1 op = case reads a1 of
      (i, ""):[] -> return (op { Op.width = Op.ReportWidth i })
      _ -> Ex.throw . BadWidthArg $ a1

-- | Turns a field on if it is True.
fieldOn ::
  Fl.T Bool
  -- ^ Fields as seen so far

  -> Fl.T Bool
  -- ^ Record that should have one True element indicating a field
  -- name seen on the command line; other elements should be False
  
  -> Fl.T Bool
  -- ^ Fields as seen so far, with new field added

fieldOn old new = (||) <$> old <*> new

-- | Turns off a field if it is True.
fieldOff ::
  Fl.T Bool
  -- ^ Fields seen so far
  
  -> Fl.T Bool
  -- ^ Record that should have one True element indicating a field
  -- name seen on the command line; other elements should be False
  
  -> Fl.T Bool
  -- ^ Fields as seen so far, with new field added

fieldOff old new = f <$> old <*> new
  where
    f o False = o
    f _ True = False

parseField :: String -> Ex.Exceptional Error (Fl.T Bool)
parseField str =
  let lower = map toLower str
      checkField s =
        if (map toLower s) == lower
        then (s, True)
        else (s, False)
      flds = checkField <$> fieldNames
  in checkFields flds

-- | Checks the fields with the True value to ensure there is only one.
checkFields :: Fl.T (String, Bool) -> Ex.Exceptional Error (Fl.T Bool)
checkFields fs =
  let f (s, b) ls = if b then s:ls else ls
  in case F.foldr f [] fs of
    [] -> Ex.throw NoMatchingFieldName
    _:[] -> return (snd <$> fs)
    ls -> Ex.throw . MultipleMatchingFieldNames $ ls



fieldNames :: Fl.T String
fieldNames = Fl.T {
  Fl.globalTransaction = "globalTransaction"
  , Fl.revGlobalTransaction = "revGlobalTransaction"
  , Fl.globalPosting = "globalPosting"
  , Fl.revGlobalPosting = "revGlobalPosting"
  , Fl.fileTransaction = "fileTransaction"
  , Fl.revFileTransaction = "revFileTransaction"
  , Fl.filePosting = "filePosting"
  , Fl.revFilePosting = "revFilePosting"
  , Fl.filtered = "filtered"
  , Fl.revFiltered = "revFiltered"
  , Fl.sorted = "sorted"
  , Fl.revSorted = "revSorted"
  , Fl.visible = "visible"
  , Fl.revVisible = "revVisible"
  , Fl.lineNum = "lineNum"
  , Fl.date = "date"
  , Fl.flag = "flag"
  , Fl.number = "number"
  , Fl.payee = "payee"
  , Fl.account = "account"
  , Fl.postingDrCr = "postingDrCr"
  , Fl.postingCmdty = "postingCmdty"
  , Fl.postingQty = "postingQty"
  , Fl.totalDrCr = "totalDrCr"
  , Fl.totalCmdty = "totalCmdty"
  , Fl.totalQty = "totalQty"
  , Fl.tags = "tags"
  , Fl.memo = "memo"
  , Fl.filename = "filename" }



{-
parseCommand ::
  DateTime
  -> S.Runtime
  -> (Op.T)
  -> ParserE Error (Op.T)
parseCommand dt rt op =
    (nextWordIs (pack "postings")
     <|> nextWordIs (pack "pos"))
    *> parseArgs dt rt op
  
parseArgs ::
  DateTime
  -> S.Runtime
  -> (Op.T)
  -> ParserE Error (Op.T)
parseArgs dt rt op =
  option op $ do
    rs <- runUntilFailure (parseArg dt rt) op
    if null rs then return op else return (last rs)

parseArg ::
  DateTime
  -> S.Runtime
  -> (Op.T)
  -> ParserE Error (Op.T)
parseArg dt rt op =
  wrapLiberty dt op 
  <|> wrapColor rt op
  <|> wrapBackground op
  <|> wrapWidth op
  <|> showField op
  <|> hideField op
  <|> showAllFields op
  <|> hideAllFields op
  <|> showZeroBalances op
  <|> hideZeroBalances op

wrapLiberty ::
  DateTime
  -> Op.T
  -> ParserE Error (Op.T)
wrapLiberty dt op = let
  dtz = Op.timeZone op
  rg = Op.radGroup op
  in fromLibertyState op
     <$> LF.parseOption dtz dt rg (toLibertyState op)

wrapBackground :: (Op.T) -> ParserE Error (Op.T)
wrapBackground st = mkSt <$> background where
  mkSt b = st { Op.drCrColors = fst b 
              , Op.baseColors = snd b }

wrapWidth :: (Op.T) -> ParserE Error (Op.T)
wrapWidth st = mkSt <$> widthArg where
  mkSt w = st { Op.width = w }

showField :: (Op.T) -> ParserE Error (Op.T)
showField st = mkSt <$> fieldArg "show" where
  mkSt f = st { Op.fields = fields' } where
    oldFields = Op.fields st
    fields' = f True oldFields

hideField :: (Op.T) -> ParserE Error (Op.T)
hideField st = mkSt <$> fieldArg "hide" where
  mkSt f = st { Op.fields = fields' } where
    oldFields = Op.fields st
    fields' = f False oldFields

toLibertyState :: (Op.T) -> (LF.State Numbered.T)
toLibertyState op =
  LF.State { LF.sensitive = Op.sensitive op
           , LF.factory = Op.factory op
           , LF.tokens = Op.tokens op
           , LF.postFilter = Op.postFilter op }

fromLibertyState :: (Op.T) -> LF.State Numbered.T -> (Op.T)
fromLibertyState op lf =
  op  { Op.sensitive = LF.sensitive lf
      , Op.factory = LF.factory lf
      , Op.tokens = LF.tokens lf
      , Op.postFilter = LF.postFilter lf }


showAllFields :: Op.T -> ParserE Error Op.T
showAllFields op =
  longNoArg (makeLongOpt . pack $ "show-all")
  *> pure (op { Op.fields = pure True })

hideAllFields :: Op.T -> ParserE Error Op.T
hideAllFields op =
  longNoArg (makeLongOpt . pack $ "hide-all")
  *> pure (op { Op.fields = pure False })

showZeroBalances :: Op.T -> ParserE Error Op.T
showZeroBalances op = op' <$ opt where
  op' = op { Op.showZeroBalances = CO.ShowZeroBalances True }
  opt = longNoArg (makeLongOpt . pack $ "show-zero-balances")

hideZeroBalances :: Op.T -> ParserE Error Op.T
hideZeroBalances op = op' <$ opt where
  op' = op { Op.showZeroBalances = CO.ShowZeroBalances False }
  opt = longNoArg (makeLongOpt . pack $ "hide-zero-balances")

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
-}
