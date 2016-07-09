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

-- | Permits constructin of a labeled tree when the label is known at
-- compile time.  The resulting expression has type
-- 'Maybe' ('Forest' 'Char' '()') -> 'Tree' 'Char' '()'.
qLabeled :: Q.QuasiQuoter
qLabeled = expOnly $ \s -> do
  let sTxt = Seq.fromList . X.unpack . X.strip . X.pack $ s
  us <- case cString sTxt of
    Left unq -> return unq
    Right _ -> fail $ "invalid label: " ++ s
  let scalar = Scalar'UnquotedString us
  [| cTree $(liftData scalar) |]
