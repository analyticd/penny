-- | Parses the global portion of the command line.
module Penny.Zinc.Parser.Filter
  ( GlobalResult(..)
  , FilterOpts(..)
  , allOpts
  , processGlobal
  ) where

import qualified Control.Monad.Trans.State as St
import Data.Maybe (mapMaybe, catMaybes)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Monoid (mconcat)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import qualified System.Console.MultiArg as MA

import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as X

import qualified Penny.Zinc.Parser.Defaults as Defaults

--
-- OptResult, and functions dealing with it
--
data OptResult
  = ROperand (L.DateTime
             -> M.CaseSensitive
             -> Ly.MatcherFactory
             -> Ex.Exceptional String Ly.Operand)
  | RPostFilter (Ex.Exceptional String Ly.PostFilterFn)
  | RMatcherSelect Ly.MatcherFactory
  | RCaseSelect M.CaseSensitive
  | ROperator (Ly.Token (L.PostFam -> Bool))
  | RSortSpec (Ex.Exceptional String Ly.Orderer)
  | RHelp

isHelp :: OptResult -> Bool
isHelp o = case o of { RHelp -> True; _ -> False }

getPostFilters
  :: [OptResult]
  -> Ex.Exceptional String [Ly.PostFilterFn]
getPostFilters =
  sequence
  . mapMaybe f
  where
    f o = case o of
      RPostFilter pf -> Just pf
      _ -> Nothing

getSortSpec
  :: [OptResult]
  -> Ex.Exceptional String Ly.Orderer
getSortSpec =
  fmap mconcat
  . sequence
  . mapMaybe f
  where
    f o = case o of
      RSortSpec x -> Just x
      _ -> Nothing

type Factory = M.CaseSensitive
             -> Text -> Ex.Exceptional Text (Text -> Bool)

makeToken
  :: L.DateTime
  -> OptResult
  -> St.State (M.CaseSensitive, Factory)
              (Maybe (Ex.Exceptional String (Ly.Token (L.PostFam -> Bool))))
makeToken dt o = case o of
  ROperand f -> do
    (s, fty) <- St.get
    let g = fmap h (f dt s fty)
        h (X.Operand fn) = Ly.TokOperand fn
    return (Just g)
  RMatcherSelect f -> do
    (c, _) <- St.get
    St.put (c, f)
    return Nothing
  RCaseSelect c -> do
    (_, f) <- St.get
    St.put (c, f)
    return Nothing
  ROperator t -> return . Just . return $ t
  _ -> return Nothing


makeTokens
  :: Defaults.T
  -> [OptResult]
  -> Ex.Exceptional String ( [Ly.Token (L.PostFam -> Bool)]
                           , (M.CaseSensitive, Factory) )
makeTokens df os =
  let initSt = (Defaults.sensitive df, Defaults.factory df)
      lsSt = mapM (makeToken (Defaults.currentTime df)) os
      (ls, st') = St.runState lsSt initSt
  in fmap (\xs -> (xs, st')) . sequence . catMaybes $ ls


allOpts :: [MA.OptSpec OptResult]
allOpts =
  map (fmap ROperand) Ly.operandSpecs
  ++ [fmap RPostFilter . fst $ Ly.postFilterSpecs]
  ++ [fmap RPostFilter . snd $ Ly.postFilterSpecs]
  ++ map (fmap RMatcherSelect) Ly.matcherSelectSpecs
  ++ map (fmap RCaseSelect) Ly.caseSelectSpecs
  ++ map (fmap ROperator) Ly.operatorSpecs
  ++ [(fmap RSortSpec) Ly.sortSpecs]
  ++ [MA.OptSpec ["help"] "h" (MA.NoArg RHelp)]

data GlobalResult
  = NeedsHelp
  | RunPenny FilterOpts

-- | Indicates the result of a successful parse of filtering options.
data FilterOpts =
  FilterOpts { resultFactory :: M.CaseSensitive
                            -> Text -> Ex.Exceptional Text (Text -> Bool)
           -- ^ The factory indicated, so that it can be used in
           -- subsequent parses of the same command line.

         , resultSensitive :: M.CaseSensitive
           -- ^ Indicated case sensitivity, so that it can be used in
           -- subsequent parses of the command line.

         , sorterFilterer :: [L.Transaction] -> [L.Box Ly.LibertyMeta]
           -- ^ Applied to a list of Transaction, will sort and filter
           -- the transactions and assign them LibertyMeta.
         }

processGlobal
  :: Defaults.T
  -> [OptResult]
  -> Ex.Exceptional String GlobalResult
processGlobal d os =
  if any isHelp os
  then return NeedsHelp
  else do
    postFilts <- getPostFilters os
    sortSpec <- getSortSpec os
    (toks, (rs, rf)) <- makeTokens d os
    let err = "could not parse filter expression."
    pdct <- Ex.fromMaybe err $ Ly.parsePredicate toks
    let sf = Ly.xactionsToFiltered pdct postFilts sortSpec
        fo = FilterOpts rf rs sf
    return $ RunPenny fo

