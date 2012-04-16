module Penny.Cabin.Posts.Report (report) where

import Control.Monad.Exception.Synchronous as Ex
import qualified Data.List.NonEmpty as NE
import qualified Data.Traversable as Tr
import qualified Penny.Cabin.Interface as Iface
import Penny.Cabin.Posts.Help (help)
import Penny.Cabin.Posts.Chunk (makeChunk)
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Posts.Numbered as Numbered
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Cabin.Posts.Parser as P
import qualified Penny.Cabin.Chunk as C
import Penny.Liberty.Operators (getPredicate)
import qualified Penny.Liberty.Types as LT
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Shield as S
import Text.Matchers.Text (CaseSensitive)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import System.Console.MultiArg.Prim (ParserE)
import Penny.Liberty.Error (Error)

balanceAccum :: 
  CO.ShowZeroBalances
  -> Maybe Bal.Balance
  -> LT.PostingInfo
  -> (Maybe Bal.Balance, (LT.PostingInfo, Maybe Bal.Balance))
balanceAccum (CO.ShowZeroBalances szb) mb po =
  let balThis = Bal.entryToBalance . Q.entry . LT.postingBox $ po
      balNew = case mb of
        Nothing -> balThis
        Just balOld -> Bal.addBalances balOld balThis
      balNoZeroes = Bal.removeZeroCommodities balNew
      bal' = if szb then Just balNew else balNoZeroes
  in (bal', (po, bal'))

balances ::
  CO.ShowZeroBalances
  -> NE.NonEmpty LT.PostingInfo
  -> NE.NonEmpty (LT.PostingInfo, Maybe Bal.Balance)
balances szb = snd . Tr.mapAccumL (balanceAccum szb) Nothing


numberPostings ::
  NE.NonEmpty (LT.PostingInfo, Maybe Bal.Balance)
  -> NE.NonEmpty Numbered.T
numberPostings ls = NE.reverse reversed where
  withPostingNums = NE.zipWith f ls ns where
    f (li, bal) pn = (li, bal, pn)
    ns = fmap Info.PostingNum (NE.iterate succ 0)
  reversed = NE.zipWith f wpn rpns where
    f (li, bal, pn) rpn = Numbered.T li bal pn rpn
    wpn = NE.reverse withPostingNums
    rpns = fmap Info.RevPostingNum (NE.iterate succ 0)
    
filterToVisible ::
  (LT.PostingInfo -> Bool) -- ^ Main predicate
  -> ([Numbered.T] -> [Numbered.T]) -- ^ Post filter
  -> NE.NonEmpty Numbered.T
  -> [Numbered.T]
filterToVisible p pf = pf . NE.filter p' where
  p' (Numbered.T info _ _ _) = p info


addVisibleNum :: [Numbered.T] -> [Info.T]
addVisibleNum = zipWith f nums where
  f num info = Numbered.toPostsInfo info num
  nums = map Info.VisibleNum [0..]

printReport ::
  Options.T
  -> (LT.PostingInfo -> Bool)
  -> ([Numbered.T] -> [Numbered.T])
  -> [LT.PostingInfo]
  -> Maybe XL.Text
printReport os mainPred postFilt lInfos = case NE.nonEmpty lInfos of
  Nothing -> Nothing
  Just neInfos -> let
    infos = addVisibleNum
            . filterToVisible mainPred postFilt
            . numberPostings
            . balances (O.showZeroBalances os)
            $ neInfos
    colors = Options.colorPref os
    in Just . C.bitsToText colors . makeChunk os $ infos


makeReportFunc ::
  Options.T
  -> [LT.PostingInfo]
  -> a
  -> Ex.Exceptional X.Text XL.Text
makeReportFunc o ps _ = case getPredicate (O.tokens o) of
  Nothing -> Ex.Exception (X.pack "postings: bad expression")
  Just p -> let pf = O.postFilter o in
    Ex.Success $ case printReport o p pf ps of
      Nothing -> XL.empty
      Just c -> c


makeReportParser ::
  (S.Runtime -> Options.T)
  -> S.Runtime
  -> CaseSensitive
  -> (CaseSensitive -> X.Text -> Ex.Exceptional X.Text (X.Text -> Bool))
  -> ParserE Error Iface.ReportFunc
makeReportParser rf rt c fact = do
  let opts = (rf rt) { O.sensitive = c
                     , O.factory = fact }
  opts' <- P.parseCommand (S.currentTime rt) rt opts
  let reportFunc = makeReportFunc opts'
  return reportFunc

-- | Creates a Postings report. Apply this function to your
-- customizations.
report ::
  (S.Runtime -> Options.T)
  -- ^ Function that, when applied to a a data type that holds various
  -- values that can only be known at runtime (such as the width of
  -- the screen, the TERM environment variable, and whether standard
  -- output is a terminal) returns which fields to show and the
  -- default report options. This way you can configure your options
  -- depending upon the runtime environment. (You can always ignore
  -- the runtime variable if you don't care about that stuff when
  -- configuring your options.) The fields and options returned by
  -- this function can be overridden on the command line.

  -> Iface.Report
report rf = Iface.Report help rpt where
  rpt = makeReportParser rf
