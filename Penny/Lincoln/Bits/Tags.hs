module Penny.Lincoln.Bits.Tags where

import Penny.Lincoln.Groups.TextNonEmpty (TextNonEmpty)

newtype Tag = Tag { unTag :: TextNonEmpty }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord)

