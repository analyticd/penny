module Penny.Copper.Util where

import qualified Data.Char as C
import Data.Ix (range)
import qualified Data.Set as S

inCat :: C.GeneralCategory -> C.GeneralCategory
         -> Char -> Bool
inCat g1 g2 c = C.generalCategory c `S.member` gs where
  gs = S.fromList (range (g1, g2))
