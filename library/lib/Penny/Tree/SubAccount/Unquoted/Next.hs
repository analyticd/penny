module Penny.Tree.SubAccount.Unquoted.Next where

import qualified Penny.Tree.SubAccount.Unquoted.Char.Next as NextC
import Data.Sequence (Seq)
import qualified Penny.Tree.Colon as Colon
import Text.Parsec.Text
import Control.Applicative
import qualified Data.Sequence as S
import Data.Foldable (toList)
import qualified Data.Text as X
import qualified Penny.Core.SubAccount as SubAccount

data T = T
  { colon :: Colon.T
  , first :: NextC.T
  , rest :: Seq NextC.T
  } deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Colon.parser <*> NextC.parser
  <*> fmap S.fromList (many NextC.parser)

toCore :: T -> SubAccount.T
toCore (T _ n1 ns)
  = SubAccount.T
  $ NextC.toChar n1 `X.cons`
  (X.pack . toList . fmap NextC.toChar $ ns)
