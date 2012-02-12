module Penny.Denver.Pretty where

import Control.Applicative (pure, (<*>))
import Data.Foldable (toList)
import Data.Text (unpack)
import Data.Time (formatTime)
import System.Locale (defaultTimeLocale)
import Text.Parsec.Error (ParseError)
import Text.PrettyPrint (
  punctuate, char, hcat, (<>), (<+>), text, Doc,
  brackets, quotes, parens, sep, hang, vcat,
  ($$), hsep)

import Penny.Copper as Cop
import qualified Penny.Copper.Meta as CM
import qualified Penny.Copper.Comments.Multiline as CM
import qualified Penny.Copper.Comments.SingleLine as CS
import qualified Penny.Copper.Item as I
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Boxes as X
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import Penny.Lincoln.Family.Siblings (Siblings(Siblings))
import qualified Penny.Lincoln.Meta as LM
import Penny.Lincoln.TextNonEmpty(TextNonEmpty(TextNonEmpty))
import qualified Penny.Lincoln.Transaction as T

class Pretty a where
  pretty :: a -> Doc

maybePretty :: (Pretty a) => String -> Maybe a -> Doc
maybePretty s m = case m of
  Nothing -> char '<' <> text ("no " ++ s) <> char '>'
  (Just i) -> pretty i

indent :: Int
indent = 2

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

instance Pretty LM.Line where
  pretty (LM.Line l) = text . show $ l

instance Pretty LM.Side where
  pretty LM.CommodityOnLeft = text "on Left"
  pretty LM.CommodityOnRight = text "on Right"

instance Pretty LM.SpaceBetween where
  pretty LM.SpaceBetween = text "space between"
  pretty LM.NoSpaceBetween = text "no space between"

instance Pretty LM.Format where
  pretty (LM.Format c s b) = sep [pretty c, pretty s, pretty b]

instance Pretty LM.Filename where
  pretty (LM.Filename f) = text "Filename:" <+> text (unpack f)

instance (Pretty a, Pretty b)
         => Pretty (LM.TransactionMeta a b) where
  pretty (LM.TransactionMeta f) =
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
instance Pretty CM.PriceLine where
  pretty (CM.PriceLine l) = text "Price line:" <+> pretty l 

instance Pretty CM.PostingLine where
  pretty (CM.PostingLine l) = text "Posting line:" <+> pretty l

instance Pretty CM.TopMemoLine where
  pretty (CM.TopMemoLine l) = text "Top memo line:" <+> pretty l

instance Pretty CM.TopLineLine where
  pretty (CM.TopLineLine l) = text "Top line line:" <+> pretty l

instance Pretty CM.PriceMeta where
  pretty (CM.PriceMeta l f) = text "Price meta:"
                             <+> pretty l <+> pretty f

instance Pretty CM.PostingMeta where
  pretty (CM.PostingMeta l mf) =
    text "Posting meta:" <+> pretty l
    <+> maybePretty "posting format" mf

instance Pretty CM.TransactionMeta where
  pretty (CM.TransactionMeta tml tl f) =
    text "Transaction meta:" <+> maybePretty "Top memo line" tml
    <+> pretty tl <+> pretty f

instance Pretty CM.Multiline where
  pretty (CM.Multiline is) = text "Multiline" <+> (hsep (map pretty is))

instance Pretty CM.Item where
  pretty (CM.Text t) = pretty t
  pretty (CM.Nested ml) = pretty ml

instance Pretty CS.Comment where
  pretty (CS.Comment t) = text (unpack t)

instance Pretty I.Item where
  pretty i = hang (text "Item") indent $ case i of
    (I.Transaction t) -> hang (text "Transaction") indent $ pretty t
    (I.Price pb) -> hang (text "Price box") indent $ pretty pb
    (I.Multiline m) -> hang (text "Multiline") indent $ pretty m
    (I.SingleLine c) -> hang (text "Single line") indent $ pretty c
    (I.BlankLine) -> text "Blank line"

instance Pretty Cop.Item where
  pretty i = hang (text "Item") indent $ case i of
    (Cop.Transaction t) -> hang (text "Transaction") indent $ pretty t
    (Cop.Price pb) -> hang (text "Price box") indent $ pretty pb

instance Pretty Cop.Ledger where
  pretty (Cop.Ledger ls) =
    hang (text "Ledger") indent $
    vcat (map toDoc ls)
    where
      toDoc (l, i) = text "line no:" <+> pretty l
                     <+> text "item:" <+> pretty i
                         
