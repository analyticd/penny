module Penny.Copper.Convert.Collect where

import qualified Penny.Copper.Tree.Memo.Transaction as TM
import qualified Penny.Copper.Tree.Memo.Posting as PM
import qualified Penny.Copper.Convert.Locate as L
import qualified Penny.Copper.Tree.TopLine as T
import qualified Penny.Copper.Tree.Posting as T
import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Copper.Convert.Serialize as Z
import Penny.Serial
import qualified Penny.Common as C

-- | Currently in a TopLineMemo
newtype TopLineMemos = TopLineMemos
  { unTopLineMemos :: Seq (L.Located TM.Memo) }
  deriving (Eq, Ord, Show)

addTopLineMemo :: TopLineMemos -> (L.Located TM.Memo) -> TopLineMemos
addTopLineMemo (TopLineMemos s) p = TopLineMemos $ s |> p

-- | Just saw a TopLine, waiting for a Posting
data AfterTopLine = AfterTopLine
  { atlMemos :: TopLineMemos
  , atlTopLine :: L.Located T.TopLine
  , atlTopLineGlobal :: Serial
  , atlTopLineClxn :: Serial
  } deriving (Eq, Ord, Show)

data PostingBox = PostingBox
  { pbPosting :: L.Located T.Posting
  , pbGlobal :: Serial
  , pbClxn :: Serial
  , pbMemos :: Seq (L.Located PM.Memo)
  } deriving (Eq, Ord, Show)

-- | At least one posting has already been seen; currently
-- processing additional postings or memo lines.
data AfterPosting = AfterPosting
  { apTopLine :: AfterTopLine
  , apPostings :: Seq PostingBox
  , apLast :: PostingBox
  } deriving (Eq, Ord, Show)

data State
  = S0
  -- ^ Not currently in posting or transaction; awaiting next input
  | S1 TopLineMemos
  | S2 AfterTopLine
  | S3 AfterPosting
  deriving (Eq, Ord, Show)

data Error
  = PostingWithoutTopLine
  | TopLineMemoWithoutTopLine
  | PostingMemoWithoutPosting
  deriving (Eq, Ord, Show)

data Good
  = NoOutput
  | OutTopLine AfterTopLine
  | OutPostings AfterPosting
  deriving (Eq, Ord, Show)

process
  :: L.Located Z.Item2
  -> State
  -> (State, Either (L.Located Error) Good)
process (L.Located loc itm) st = case st of
  S0 -> procClear loc itm
  S1 tlm -> procTopLineMemos tlm loc itm
  S2 atl -> procAfterTopLine atl loc itm
  S3 afp -> procAfterPosting afp loc itm

procClear
  :: C.Location
  -> Z.Item2
  -> (State, Either (L.Located Error) Good)
procClear loc itm = case itm of
  Z.Item2TxnMemo m ->
    ( S1 . TopLineMemos . singleton . L.Located loc $ m
    , Right NoOutput)
  Z.Item2TopLine tl s1 s2 ->
    ( S2 $ AfterTopLine { atlMemos = TopLineMemos S.empty
                        , atlTopLine = L.Located loc tl
                        , atlTopLineGlobal = s1
                        , atlTopLineClxn = s2 }
    , Right NoOutput )

  Z.Item2Posting _ _ _ ->
    (S0, Left (L.Located loc PostingWithoutTopLine))

  Z.Item2PstgMemo _ ->
    (S0, Left (L.Located loc PostingMemoWithoutPosting))



procTopLineMemos
  :: TopLineMemos
  -> C.Location
  -> Z.Item2
  -> (State, Either (L.Located Error) Good)
procTopLineMemos tlm loc itm = case itm of
  Z.Item2TxnMemo m ->
    (S1 $ addTopLineMemo tlm (L.Located loc m), Right NoOutput)
  Z.Item2TopLine tl s1 s2 ->
    ( S2 $ AfterTopLine { atlMemos = tlm
                        , atlTopLine = L.Located loc tl
                        , atlTopLineGlobal = s1
                        , atlTopLineClxn = s2
                        }
    ,  Right NoOutput)
  Z.Item2Posting _ _ _ ->
    ( S0, Left (L.Located loc PostingWithoutTopLine))
  Z.Item2PstgMemo _ ->
    ( S0, Left (L.Located loc PostingMemoWithoutPosting))


procAfterTopLine
  :: AfterTopLine
  -> C.Location
  -> Z.Item2
  -> (State, Either (L.Located Error) Good)
procAfterTopLine atl loc itm = case itm of
  Z.Item2TxnMemo m ->
    ( S1 $ TopLineMemos (singleton (L.Located loc m))
    ,  Right $ OutTopLine atl )
  Z.Item2TopLine tl' s1' s2' ->
    ( S2 $ AfterTopLine { atlMemos = TopLineMemos S.empty
                        , atlTopLine = L.Located loc tl'
                        , atlTopLineGlobal = s1'
                        , atlTopLineClxn = s2' }
    , Right $ OutTopLine atl )
  Z.Item2Posting p s1' s2' ->
    ( S3 $ AfterPosting atl S.empty pb, Right NoOutput )
    where
      pb = PostingBox { pbPosting = L.Located loc p
                      , pbGlobal = s1'
                      , pbClxn = s2'
                      , pbMemos = S.empty
                      }
  Z.Item2PstgMemo _ ->
    ( S0, Left (L.Located loc PostingMemoWithoutPosting ))

procAfterPosting
  :: AfterPosting
  -> C.Location
  -> Z.Item2
  -> (State, Either (L.Located Error) Good)
procAfterPosting afp loc itm = case itm of
  Z.Item2TxnMemo tm ->
    ( S1 . TopLineMemos . S.singleton . L.Located loc $ tm
    , Right . OutPostings $ afp )
  Z.Item2TopLine tl s1 s2 ->
    ( S2 $ AfterTopLine { atlMemos = TopLineMemos S.empty
                        , atlTopLine = L.Located loc tl
                        , atlTopLineGlobal = s1
                        , atlTopLineClxn = s2 }
    , Right . OutPostings $ afp )
  Z.Item2Posting p s1 s2 ->
    ( S3 $ AfterPosting { apTopLine = apTopLine afp
                        , apPostings = apPostings afp |> apLast afp
                        , apLast = PostingBox
                          { pbPosting = L.Located loc p
                          , pbGlobal = s1
                          , pbClxn = s2
                          , pbMemos = S.empty
                          }
                        }
    , Right NoOutput )
  Z.Item2PstgMemo m ->
    ( S3 $ afp { apLast = (apLast afp)
                { pbMemos = (pbMemos (apLast afp)) |>
                  L.Located loc m } }
    , Right NoOutput )

destroy
  :: State
  -> Either Error Good
destroy st = case st of
  S0 -> Right NoOutput
  S1 _ -> Left TopLineMemoWithoutTopLine
  S2 atl -> Right $ OutTopLine atl
  S3 app -> Right $ OutPostings app

data Result = Result
  { resClxn :: C.Clxn
  , resErrors :: Seq (L.Located Error)
  , resFinalError :: Maybe Error
  , resGoods :: Seq (Either AfterTopLine AfterPosting)
  } deriving (Eq, Ord, Show)

runner
  :: Seq (L.Located Z.Item2)
  -> ( Maybe Error
     , Seq (L.Located Error)
     , Seq (Either AfterTopLine AfterPosting)
     )
runner items = (finalErr, errors, allGoods)
  where
    (stFin, errors, goods) = go items S0 S.empty S.empty
    go sq st es gs = case viewl sq of
      EmptyL -> (st, es, gs)
      x :< xs -> go xs st' es' gs'
        where
          (st', ei) = process x st
          (es', gs') = case ei of
            Left err -> (es |> err, gs)
            Right g -> (es, appendGood gs g)
    (finalErr, allGoods) = case destroy stFin of
      Left e -> (Just e, goods)
      Right g -> (Nothing, appendGood goods g)

appendGood
  :: Seq (Either AfterTopLine AfterPosting)
  -> Good
  -> Seq (Either AfterTopLine AfterPosting)
appendGood sq g = case g of
  NoOutput -> sq
  OutTopLine atl -> sq |> (Left atl)
  OutPostings afp -> sq |> (Right afp)

runPackage :: Z.Package2 -> Result
runPackage (Z.Package2 cx sq) = Result cx es fin gs
  where
    (fin, es, gs) = runner sq

newtype Results = Results { unResults :: Seq Result }
  deriving (Eq, Ord, Show)


collect :: Z.Packages2 -> Results
collect = Results . fmap runPackage . Z.unPackages2
