module Penny.Copper.Render where

import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.List (intersperse)
import qualified Data.Text as X
import Data.Text (Text)
import qualified Penny.Copper.Terminals as T
import qualified Penny.Lincoln as L

-- | Is True if a sub account can be rendered at Level 1;
-- False otherwise.
isSubAccountLvl1 :: L.SubAccount -> Bool
isSubAccountLvl1 (L.SubAccount x) =
  X.all T.lvl1AcctChar x && not (X.null x)

isAccountLvl1 :: L.Account -> Bool
isAccountLvl1 (L.Account ls) =
  (not . null $ ls)
  && (all isSubAccountLvl1 ls)

lvl1Account :: L.Account -> Maybe Text
lvl1Account a@(L.Account ls) = do
  guard (isAccountLvl1 a)
  let txt = X.concat . intersperse (X.singleton ':')
            . map L.unSubAccount $ ls
  return $ '{' `X.cons` txt `X.snoc` '}'

isFirstSubAccountLvl2 :: L.SubAccount -> Bool
isFirstSubAccountLvl2 (L.SubAccount x) = case X.uncons x of
  Nothing -> False
  Just (c, r) -> T.letter c && (X.all T.lvl2AcctOtherChar r)

isOtherSubAccountLvl2 :: L.SubAccount -> Bool
isOtherSubAccountLvl2 (L.SubAccount x) =
  (not . X.null $ x)
  && (X.all T.lvl2AcctOtherChar x)

isAccountLvl2 :: L.Account -> Bool
isAccountLvl2 (L.Account ls) = case ls of
  [] -> False
  x:xs -> isFirstSubAccountLvl2 x && all isOtherSubAccountLvl2 xs

lvl2Account :: L.Account -> Maybe Text
lvl2Account a@(L.Account ls) = do
  guard $ isAccountLvl2 a
  return . X.concat . intersperse (X.singleton ':')
         . map L.unSubAccount $ ls

-- | Shows an account, with the minimum level of quoting
-- possible. Fails with an error if any one of the characters in the
-- account name does not satisfy the 'lvl1Char' predicate. Otherwise
-- returns a rendered account, quoted if necessary.
account :: L.Account -> Maybe Text
account a = lvl2Account a <|> lvl1Account a

