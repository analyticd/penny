module Penny.Lincoln.Pretty where

import Control.Applicative (pure, (<*>))
import Data.Foldable (toList)
import Data.Text (unpack)
import Data.Time (formatTime)
import System.Locale (defaultTimeLocale)
import Text.Parsec.Error (ParseError)
import Text.PrettyPrint (
  punctuate, char, hcat, (<>), (<+>), text, Doc,
  brackets, quotes, parens, sep, hang, vcat,
  ($$))

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Boxes as X
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import Penny.Lincoln.Family.Siblings (Siblings(Siblings))
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.TextNonEmpty(TextNonEmpty(TextNonEmpty))
import qualified Penny.Lincoln.Transaction as T

class Pretty a where
  pretty :: a -> Doc

instance Pretty B.SubAccountName where
  pretty (B.SubAccountName t) = pretty t

instance Pretty B.Account where
  pretty (B.Account ss) =
    hcat
    . punctuate (char ':')
    $ (map pretty . toList $ ss)

instance Pretty B.Amount where
  pretty (B.Amount q c) = pretty q <+> pretty c

instance Pretty B.Commodity where
  pretty (B.Commodity cs) =
    hcat
    . punctuate (char ':')
    $ (map pretty . toList $ cs)

instance Pretty B.SubCommodity where
  pretty (B.SubCommodity t) = pretty t

instance Pretty B.DateTime where
  pretty (B.DateTime t) =
    text
    . formatTime defaultTimeLocale fmt
    $ t
    where
      fmt = "%F %T %z"
                          
instance Pretty B.DrCr where
  pretty B.Debit = text "Dr"
  pretty B.Credit = text "Cr"

instance Pretty B.Entry where
  pretty (B.Entry d a) = pretty d <+> pretty a

instance Pretty B.Qty where
  pretty = text . show . B.unQty

instance Pretty TextNonEmpty where
  pretty (TextNonEmpty c cs) = char c <> text (unpack cs)

instance Pretty B.Flag where
  pretty (B.Flag t) = brackets . pretty $ t

instance Pretty B.Memo where
  pretty (B.Memo t) = text "Memo: " <> (quotes . pretty $ t)

instance Pretty B.Number where
  pretty (B.Number t) = parens . pretty $ t

instance Pretty B.Payee where
  pretty (B.Payee t) = text "Payee: " <> (quotes. pretty $ t)

instance Pretty B.From where
  pretty (B.From c) = pretty c

instance Pretty B.To where
  pretty (B.To c) = pretty c

instance Pretty B.CountPerUnit where
  pretty (B.CountPerUnit q) = pretty q

instance Pretty B.Price where
  pretty p =
    text "Price from "
    <> pretty (B.from p)
    <> text " to "
    <> pretty (B.to p)
    <> text " at "
    <> pretty (B.countPerUnit p)

instance Pretty B.PricePoint where
  pretty (B.PricePoint d p) =
    text "Price point: "
    <+> pretty d
    <+> pretty p

instance Pretty B.Tag where
  pretty (B.Tag t) = char '#' <> pretty t

instance Pretty B.Tags where
  pretty (B.Tags ts) = hcat . map pretty $ ts

instance Pretty Doc where
  pretty = id

maybePretty :: (Pretty a) => String -> Maybe a -> Doc
maybePretty s m = case m of
  Nothing -> char '<' <> text ("no " ++ s) <> char '>'
  (Just i) -> pretty i


instance Pretty T.Posting where
  pretty t =
    sep $ text "Posting:" : ls where
    ls = [ maybePretty "payee"  . T.pPayee
         , maybePretty "number" . T.pNumber
         , maybePretty "flag"   . T.pFlag
         , pretty               . T.pAccount
         , pretty               . T.pTags
         , pretty               . T.pEntry
         , maybePretty "memo"   . T.pMemo ]
         <*> pure t

instance Pretty T.TopLine where
  pretty t = sep $ text "TopLine:" : ls where
    ls = [ pretty . T.tDateTime
         , maybePretty "flag"   . T.tFlag
         , maybePretty "number" . T.tNumber
         , maybePretty "payee"  . T.tPayee
         , maybePretty "memo"   . T.tMemo ]
         <*> pure t

indent :: Int
indent = 2

instance (Pretty p, Pretty c)
         => Pretty (F.Family p c) where
  pretty (F.Family p c1 c2 cs) =
    hang (text "Family") indent
    $ hang (text "Parent") indent (pretty p)
    $$ vcat (map toChild (zip ([1..] :: [Int]) (c1:c2:cs)))
    where
      toChild (i, c) =
        hang (text ("Child " ++ show i)) indent $ pretty c

instance (Pretty p, Pretty c)
         => Pretty (C.Child p c) where
  pretty (C.Child t s ss p) =
    hang (text "Child") indent
    $ hang (text "This child") indent (pretty t)
    $$ hang (text "Parent") indent (pretty p)
    $$ vcat (map toSibling (zip ([1..] :: [Int]) (s:ss)))
    where
      toSibling (i, c) =
        hang (text ("Sibling " ++ show i)) indent $ pretty c

instance Pretty a => Pretty (Siblings a) where
  pretty (Siblings f s rs) =
    hang (text "Siblings") indent
    $ vcat (map toSibling (zip ([1..] :: [Int]) (f:s:rs)))
    where
      toSibling (i, c) =
        hang (text ("Sibling " ++ show i)) indent $ pretty c

instance Pretty T.Transaction where
  pretty t = text "Transaction:" <+> pretty (T.unTransaction t)

instance Pretty M.Line where
  pretty (M.Line l) = text . show $ l

instance Pretty M.Side where
  pretty M.CommodityOnLeft = text "on Left"
  pretty M.CommodityOnRight = text "on Right"

instance Pretty M.SpaceBetween where
  pretty M.SpaceBetween = text "space between"
  pretty M.NoSpaceBetween = text "no space between"

instance Pretty M.Format where
  pretty (M.Format c s b) = sep [pretty c, pretty s, pretty b]

instance Pretty M.Filename where
  pretty (M.Filename f) = text "Filename:" <+> text (unpack f)

instance (Pretty a, Pretty b)
         => Pretty (M.TransactionMeta a b) where
  pretty (M.TransactionMeta f) =
    hang (text "Transaction meta") indent (pretty f)

instance (Pretty t, Pretty p)
         => Pretty (X.TransactionBox t p) where
  pretty box = hang (text "Transaction box:") indent $
               pretty (X.transaction box)
               $$ maybePretty "transaction meta" (X.transactionMeta box)

instance (Pretty t, Pretty p)
         => Pretty (X.PostingBox t p) where
  pretty box = hang (text "Posting box:") indent $
               pretty (X.postingBundle box)
               $$ maybePretty "posting meta" (X.metaBundle box)

instance (Pretty m) => Pretty (X.PriceBox m) where
  pretty box = hang (text "Price box:") indent $
               pretty (X.price box)
               $$ maybePretty "price meta" (X.priceMeta box)

instance (Pretty l, Pretty r)
         => Pretty (Either l r) where
  pretty (Left l) = text "Left" <+> pretty l
  pretty (Right r) = text "Right" <+> pretty r

instance Pretty ParseError where
  pretty e = text "Parse error" <+> pretty (text . show $ e)
