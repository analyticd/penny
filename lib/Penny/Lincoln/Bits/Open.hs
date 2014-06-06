-- | These are the bits that are /open/; that is, their constructors
-- are exported. This includes most bits. Some bits that have open
-- constructors are not in this module because they include other bits
-- that do not have exported constructors.

module Penny.Lincoln.Bits.Open where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as X
import Penny.Lincoln.Equivalent
import qualified Penny.Lincoln.Serial as S
import Penny.Lincoln.Decimal
import Data.Bifunctor
import Data.Monoid ((<>))

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord)

-- | Tags are equivalent if they have the same tags (even if in a
-- different order).
instance Equivalent Tags where
  equivalent (Tags t1) (Tags t2) = sort t1 == sort t2
  compareEv (Tags t1) (Tags t2) =
    compare (sort t1) (sort t2)

-- Metadata

