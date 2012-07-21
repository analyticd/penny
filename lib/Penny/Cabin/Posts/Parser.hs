module Penny.Cabin.Posts.Parser (parseOptions) where

import Control.Applicative ((<|>), (<$>), pure, many, (<*>))
import Control.Monad ((>=>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toLower)
import qualified Data.Foldable as F
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
import qualified Penny.Liberty.Expressions as Exp
import qualified Penny.Shield as S

data Error = BadColorName String
             | BadBackgroundArg String
             | BadWidthArg String
             | NoMatchingFieldName
             | MultipleMatchingFieldNames [String]
             | LibertyError Ly.Error
             deriving Show

-- | Parses the command line from the first word remaining up until,
-- but not including, the first non-option argment.
parseOptions ::
  S.Runtime
  -> Op.T
  -- ^ Default options for the posts report
  -> Parser (Ex.Exceptional Error Op.T)
parseOptions rt op = fmap f (many (parseOption rt))
  where
    folder = foldl (>=>) return
    f ls = (folder ls) op


parseOption ::
  S.Runtime
  -> Parser (Op.T -> Ex.Exceptional Error Op.T)
parseOption rt =
  operand rt
  <|> postFilter
  <|> matcherSelect
  <|> caseSelect
  <|> operator
  <|> color rt
  <|> background
  <|> width
  <|> showField
  <|> hideField
  <|> showAllFields
  <|> hideAllFields
  <|> showZeroBalances
  <|> hideZeroBalances

operand :: S.Runtime -> Parser (Op.T -> Ex.Exceptional Error Op.T)
operand rt = f <$> Ly.parseOperand
  where
    f lyFn op =
      let dtz = Op.timeZone op
          rg = Op.radGroup op
          dt = S.currentTime rt
          cs = Op.sensitive op
          fty = Op.factory op
      in case lyFn dt dtz rg cs fty of
        Ex.Exception e -> Ex.throw . LibertyError $ e
        Ex.Success (Exp.Operand g) ->
          let ts' = Op.tokens op ++ [Exp.TokOperand g]
          in return op { Op.tokens = ts' }


postFilter :: Parser (Op.T -> Ex.Exceptional Error Op.T)
postFilter = f <$> Ly.parsePostFilter
  where
    f ex op =
      case ex of
        Ex.Exception e -> Ex.throw . LibertyError $ e
        Ex.Success pf ->
          return op { Op.postFilter = Op.postFilter op ++ [pf] }

matcherSelect :: Parser (Op.T -> Ex.Exceptional Error Op.T)
matcherSelect = f <$> Ly.parseMatcherSelect
  where
    f mf op = return op { Op.factory = mf }

caseSelect :: Parser (Op.T -> Ex.Exceptional Error Op.T)
caseSelect = f <$> Ly.parseCaseSelect
  where
    f cs op = return op { Op.sensitive = cs }

operator :: Parser (Op.T -> Ex.Exceptional Error Op.T)
operator = f <$> Ly.parseOperator
  where
    f oo op = return op { Op.tokens = Op.tokens op ++ [oo] }

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

showField :: Parser (Op.T -> Ex.Exceptional Error Op.T)
showField = parseOpt ["show"] "" (C.OneArg f)
  where
    f a1 op = do
      fl <- parseField a1
      let newFl = fieldOn (Op.fields op) fl
      return op { Op.fields = newFl }

hideField :: Parser (Op.T -> Ex.Exceptional Error Op.T)
hideField = parseOpt ["hide"] "" (C.OneArg f)
  where
    f a1 op = do
      fl <- parseField a1
      let newFl = fieldOff (Op.fields op) fl
      return op { Op.fields = newFl }

showAllFields :: Parser (Op.T -> Ex.Exceptional a Op.T)
showAllFields = parseOpt ["show-all"] "" (C.NoArg f)
  where
    f op = return (op {Op.fields = pure True})

hideAllFields :: Parser (Op.T -> Ex.Exceptional a Op.T)
hideAllFields = parseOpt ["hide-all"] "" (C.NoArg f)
  where
    f op = return (op {Op.fields = pure False})

showZeroBalances :: Parser (Op.T -> Ex.Exceptional a Op.T)
showZeroBalances = parseOpt ["show-zero-balances"] "" (C.NoArg f)
  where
    f op =
      return (op {Op.showZeroBalances = CO.ShowZeroBalances True })

hideZeroBalances :: Parser (Op.T -> Ex.Exceptional a Op.T)
hideZeroBalances = parseOpt ["hide-zero-balances"] "" (C.NoArg f)
  where
    f op =
      return (op {Op.showZeroBalances = CO.ShowZeroBalances False })

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
