-- | Associates each line in each collection with its
-- associated line number.  Retains only posting memo lines,
-- transaction memo lines, transaction lines, and posting lines;
-- discards all other lines.

module Penny.Copper.Convert.Locate where

import qualified Penny.Common as C
import qualified Penny.Copper.Package as P
import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Copper.Tree.Memo.Transaction as TM
import qualified Penny.Copper.Tree.Memo.Posting as PM
import qualified Penny.Copper.Tree.Posting as T
import qualified Penny.Copper.Tree.TopLine as T
import qualified Penny.Copper.Tree.File as T
import qualified Penny.Copper.Tree.Line as T

data Item1
  = Item1TxnMemo TM.Memo
  | Item1TopLine T.TopLine
  | Item1Posting T.Posting
  | Item1PstgMemo PM.Memo
  deriving (Eq, Ord, Show)

data Located a = Located C.Location a
  deriving (Eq, Ord, Show)

instance Functor Located where
  fmap f (Located l a) = Located l (f a)

data Package1 = Package1 C.Clxn (Seq (Located Item1))
  deriving (Eq, Ord, Show)

newtype Packages1 = Packages1 { unPackages1 :: Seq Package1 }
  deriving (Eq, Ord, Show)

locate :: P.Packages -> Packages1
locate = Packages1 . fmap locatePackage . P.unPackages

locatePackage :: P.Package -> Package1
locatePackage (P.Package cx (T.File sq _)) = Package1 cx $ go 1 sq
  where
    go ix s = case viewl s of
      EmptyL -> S.empty
      x :< xs -> case x of
        T.L3 m -> process (Item1TxnMemo m)
        T.L4 t -> process (Item1TopLine t)
        T.L5 p -> process (Item1Posting p)
        T.L6 m -> process (Item1PstgMemo m)
        _ -> go ix xs
        where
          process b = Located (C.Location ix) b <| go (succ ix) xs
