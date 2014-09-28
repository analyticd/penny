module Penny.Tree.SubAccount.Unquoted.First where

import qualified Penny.Tree.SubAccount.Unquoted.Char.First as FirstC
import qualified Penny.Tree.SubAccount.Unquoted.Char.Next as NextC
import Data.Sequence (Seq)
import Control.Applicative
import qualified Data.Sequence as S
import Text.Parsec.Text
import qualified Penny.Core.SubAccount as SubAccount
import qualified Data.Text as X
import Data.Foldable (toList)

data T = T
  { first :: FirstC.T
  , rest :: Seq NextC.T
  } deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> FirstC.parser <*> fmap S.fromList (many NextC.parser)

toCore :: T -> SubAccount.T
toCore t = SubAccount.T $ FirstC.toChar (first t) `X.cons`
  (X.pack . toList . fmap NextC.toChar . rest $ t)
