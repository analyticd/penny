{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Penny.Clatch.Matcher where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Penny.Clatch
import Penny.Ledger
import Penny.Ledger.Matcher
import Penny.Matcher
import Penny.Field.Matcher
import Penny.Semantic.Matcher
import Turtle.Pattern (prefix, suffix)

-- | Creates a 'Matcher' that looks for a parent tree with the exact
-- name given.  First performs a pre-order search in the metadata of
-- the posting; then performs a pre-order search in the metadata for
-- the top line.  If successful, returns the child forest.
findNamedTree
  :: forall l. Ledger l
  => Text
  -> Matcher (Clatch l) l (Seq (TreeL l))
findNamedTree name = matchPstg <|> matchTxn
  where

    mtcr :: forall l. Ledger l => Matcher (TreeL l) l (Seq (TreeL l))
    mtcr = do
      sc <- Penny.Ledger.Matcher.scalar
      txt <- study Penny.Field.Matcher.text sc
      study (equal name) txt
      subj <- getSubject
      lift $ Penny.Ledger.offspring subj

    finder :: forall l. Ledger l => Matcher (Seq (TreeL l)) l (Seq (TreeL l))
    finder = each . preOrder $ mtcr

    matchTxn :: forall l. Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
    matchTxn = do
      txn <- fmap transactionL getSubject
      ts <- lift $ Penny.Ledger.txnMeta txn
      study finder ts

    matchPstg :: forall l. Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
    matchPstg = do
      pstg <- fmap postingL getSubject
      ts <- lift $ Penny.Ledger.pstgMeta pstg
      study finder ts

payee :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
payee = standard <|> shortcut
  where
    standard = findNamedTree "payee"
    shortcut = do
      subj <- getSubject
      trees <- lift . txnMeta . transactionL $ subj
      flip study trees . each $ do
        noOffspring
        sc <- Penny.Ledger.Matcher.scalar
        txt <- study text sc
        invert $ study (pattern (prefix "(" >> suffix ")")) txt
        fmap Seq.singleton getSubject

invert :: MonadPlus m => m a -> m ()
invert k = do
  mayR <- (liftM Just k) `mplus` (return Nothing)
  case mayR of
    Nothing -> return ()
    Just _ -> mzero
