module Penny.Core.Transaction where

import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Core.Balanced as Balanced
import qualified Penny.Core.Posting as Posting
import qualified Penny.Core.Transaction.Error as Error
import Data.Foldable (foldlM, Foldable)
import qualified Penny.Core.Ents as Ents

data T = T
  { topLine :: TopLine.T
  , postings :: Balanced.T Posting.T
  } deriving (Eq, Ord, Show)

noPostings :: TopLine.T -> T
noPostings tl = T tl Balanced.empty

fromPostings
  :: Foldable f
  => TopLine.T
  -> f Posting.T
  -> Either Error.T T
fromPostings tl ls = do
  es <- foldlM folder Ents.empty ls
  case Balanced.fromEnts es of
    Left imb -> Left $ Error.Imbalanced imb
    Right b -> return $ T tl b
  where
    folder e p = case Ents.appendTrio p e (Posting.trio p) of
      Left err -> Left $ Error.Trio err p
      Right g -> return g
