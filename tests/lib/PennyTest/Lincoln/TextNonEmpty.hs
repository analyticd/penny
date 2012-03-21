module PennyTest.Lincoln.TextNonEmpty where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Lincoln.TextNonEmpty as T
import qualified Data.Text as X
import qualified Test.QuickCheck as Q

randText :: Q.Gen X.Text
randText = X.pack <$> Q.arbitrary

-- | Generates a TextNonEmpty using the given generators for the first
-- character and for all other characters. The length of the tail
-- depends on the size parameter.
genTextNonEmpty :: Q.Gen Char -> Q.Gen Char -> Q.Gen T.TextNonEmpty
genTextNonEmpty g1 gr =
  T.TextNonEmpty
  <$> g1
  <*> (X.pack <$> Q.listOf gr)
