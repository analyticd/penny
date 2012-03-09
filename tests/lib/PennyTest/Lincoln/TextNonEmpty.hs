module PennyTest.Lincoln.TextNonEmpty where

import Control.Applicative ((<$>), (<*>))
import qualified Penny.Lincoln.TextNonEmpty as T
import qualified Data.Text as X
import qualified Test.QuickCheck as Q

randText :: Q.Gen X.Text
randText = X.pack <$> Q.arbitrary

instance Q.Arbitrary T.TextNonEmpty where
  arbitrary = T.TextNonEmpty <$> Q.arbitrary <*> randText
