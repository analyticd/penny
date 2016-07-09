{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Quasi quoters allowing the construction of trees when their
-- values are known at compile time.
module Penny.Copper.Quasi where

import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Language.Haskell.TH.Quote as Q

import Penny.Copper.Copperize
import Penny.Copper.Types
import Penny.Quasi

-- | Permits construction of a labeled 'Scalar' when the label is
-- known at compile time.  The resulting expression has type
-- 'Scalar' 'Char' '()'.
--
-- This splice will not work with GHC 7.10.  See
--
-- <https://ghc.haskell.org/trac/ghc/ticket/10796>
qLabeled :: Q.QuasiQuoter
qLabeled = expOnly $ \s -> do
  let sTxt = Seq.fromList . X.unpack . X.strip . X.pack $ s
  us <- case cString sTxt of
    Left unq -> return unq
    Right _ -> fail $ "invalid label: " ++ s
  liftData (Scalar'Label (Label cApostrophe us))
