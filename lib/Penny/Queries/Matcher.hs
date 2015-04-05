{-# LANGUAGE OverloadedStrings #-}

-- | Pattern matching.
module Penny.Queries.Matcher where

import Control.Monad.Trans.Class
import qualified Data.Foldable as F
import Control.Applicative
import Data.Text (Text)
import Penny.Lincoln.Matcher
import qualified Penny.Lincoln as L hiding (fromTo)
import qualified Penny.Ledger as L
import Data.Sequence (Seq)
import Data.Monoid
import Turtle.Pattern
import Data.String

-- # Utilities

-- | Nests a 'Matcher' within the current 'Matcher', and adds an
-- 'L.Opinion' indicating what is going on.
labelNest
  :: Monad m
  => L.Opinion
  -- ^ Descriptive text
  -> (t -> m t')
  -- ^ Convert the parent type to the nested type
  -> Matcher t' m a
  -> Matcher t  m a
labelNest op get mr = do
  L.inform ("nesting: " <> op)
  L.nest get mr

-- # Semantic

equal
  :: (L.SemanticEq s, L.Display s, Monad m)
  => s
  -> Matcher s m s
equal tgt = do
  subj <- getSubject
  let subjStr = fromString $ L.display subj ""
      tgtStr = fromString $ L.display tgt ""
  if L.semanticEq subj tgt
    then acceptL (subjStr <> " is equal to " <> tgtStr) subj
    else rejectL (subjStr <> " is not equal to " <> tgtStr)

greater
  :: (L.SemanticOrd s, L.Display s, Monad m)
  => s
  -> Matcher s m s
greater tgt = do
  subj <- getSubject
  let subjStr = fromString $ L.display subj ""
      tgtStr = fromString $ L.display tgt ""
  if L.semanticOrd subj tgt == GT
    then acceptL (subjStr <> " is greater than " <> tgtStr) subj
    else rejectL (subjStr <> " is not greater than " <> tgtStr)

less
  :: (L.SemanticOrd s, L.Display s, Monad m)
  => s
  -> Matcher s m s
less tgt = do
  subj <- getSubject
  let subjStr = fromString $ L.display subj ""
      tgtStr = fromString $ L.display tgt ""
  if L.semanticOrd subj tgt == LT
    then acceptL (subjStr <> " is less than " <> tgtStr) subj
    else rejectL (subjStr <> " is not less than " <> tgtStr)

-- # Commodities

commodityName
  :: Monad m
  => Matcher Text m a
  -> Matcher L.Commodity m a
commodityName mtcr = do
  inform "running Text matcher on commodity name"
  nest (\(L.Commodity txt) -> return txt) mtcr



-- # Prices

dateTime
  :: L.Ledger m
  => Matcher L.DateTime m a
  -> Matcher (L.PriceL m) m a
dateTime = labelNest "dateTime" L.dateTime

fromTo
  :: L.Ledger m
  => Matcher L.FromTo m a
  -> Matcher (L.PriceL m) m a
fromTo = labelNest "fromTo" L.fromTo

exchange
  :: L.Ledger m
  => Matcher L.Exch m a
  -> Matcher (L.PriceL m) m a
exchange = labelNest "exchange" L.exchange

-- # Scalars

nestField
  :: Monad m
  => L.Opinion
  -> (t -> m (Maybe t'))
  -> Matcher t' m a
  -> Matcher t m a
nestField op get mtcr = do
  curr <- getSubject
  mayT' <- lift $ get curr
  inform $ "attempting to extract field: " <> op
  case mayT' of
    Nothing -> rejectL "field not found"
    Just t' -> do
      inform "field found"
      localSubject mtcr t'


text
  :: Monad m
  => Matcher Text m a
  -> Matcher L.Scalar m a
text = nestField "text" (fmap return L.scalarChars)

date
  :: Monad m
  => Matcher L.Date m a
  -> Matcher L.Scalar m a
date = nestField "date" (fmap return L.scalarDate)

time
  :: Monad m
  => Matcher L.Time m a
  -> Matcher L.Scalar m a
time = nestField "time" (fmap return L.scalarTime)

zone
  :: Monad m
  => Matcher L.Zone m a
  -> Matcher L.Scalar m a
zone = nestField "zone" (fmap return L.scalarZone)

integer
  :: Monad m
  => Matcher Integer m a
  -> Matcher L.Scalar m a
integer = nestField "integer" (fmap return L.scalarInteger)

-- # Trees

realm
  :: L.Ledger m
  => Matcher L.Realm m a
  -> Matcher (L.TreeL m) m a
realm = labelNest "realm" L.realm


offspring
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TreeL m) m a
offspring = labelNest "offspring" L.offspring

-- | Traverses this tree and all child trees, in pre-order; that is,
-- the node is visited, followed by visiting all its child nodes.
preOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
preOrder mtcr = do
  s <- L.getSubject
  cs <- L.offspring s
  (L.inform "pre-order search - this node" >> mtcr) <|>
    (L.inform "pre-order search - children" >>
      (F.asum . fmap (L.localSubject (preOrder mtcr)) $ cs))

scalar
  :: L.Ledger m
  => Matcher L.Scalar m a
  -> Matcher (L.TreeL m) m a
scalar = nestField "scalar" L.scalar

noScalar
  :: L.Ledger m
  => Matcher (L.TreeL m) m ()
noScalar = do
  inform "testing whether tree does not have a scalar"
  nest L.scalar nothing


-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
postOrder mtcr = do
  s <- L.getSubject
  cs <- L.offspring s
  (L.inform "post-order search - children" >>
    (F.asum . fmap (L.localSubject (postOrder mtcr)) $ cs))
  <|> (L.inform "post-order search - this node" >> mtcr)

-- # Transactions

txnMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TransactionL m) m a
txnMeta = labelNest "txnMeta" L.txnMeta

topLineSer
  :: L.Ledger m
  => Matcher L.TopLineSer m a
  -> Matcher (L.TransactionL m) m a
topLineSer = labelNest "topLineSer" L.topLineSer


postings
  :: L.Ledger m
  => Matcher (Seq (L.PostingL m)) m a
  -> Matcher (L.TransactionL m) m a
postings = labelNest "postings" L.postings


-- # Postings

pstgMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.PostingL m) m a
pstgMeta = labelNest "pstgMeta" L.pstgMeta


trio
  :: L.Ledger m
  => Matcher L.Trio m a
  -> Matcher (L.PostingL m) m a
trio = labelNest "trio" L.trio


qty
  :: L.Ledger m
  => Matcher L.Qty m a
  -> Matcher (L.PostingL m) m a
qty = labelNest "qty" L.qty


commodity
  :: L.Ledger m
  => Matcher L.Commodity m a
  -> Matcher (L.PostingL m) m a
commodity = labelNest "commodity" L.commodity


postingSer
  :: L.Ledger m
  => Matcher L.PostingSer m a
  -> Matcher (L.PostingL m) m a
postingSer = labelNest "postingSer" L.postingSer



