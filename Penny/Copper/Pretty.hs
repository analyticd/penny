module Penny.Copper.Pretty where

import Data.Text (unpack)
import Text.PrettyPrint (text, (<+>), hsep, hang, vcat)

import Penny.Copper as C
import qualified Penny.Copper.Meta as M
import qualified Penny.Copper.Comments.Multiline as CM
import qualified Penny.Copper.Comments.SingleLine as CS
import qualified Penny.Copper.Item as I
import Penny.Lincoln.Pretty (Pretty(pretty), maybePretty, indent)

instance Pretty M.PriceLine where
  pretty (M.PriceLine l) = text "Price line:" <+> pretty l 

instance Pretty M.PostingLine where
  pretty (M.PostingLine l) = text "Posting line:" <+> pretty l

instance Pretty M.TopMemoLine where
  pretty (M.TopMemoLine l) = text "Top memo line:" <+> pretty l

instance Pretty M.TopLineLine where
  pretty (M.TopLineLine l) = text "Top line line:" <+> pretty l

instance Pretty M.PriceMeta where
  pretty (M.PriceMeta l f) = text "Price meta:"
                             <+> pretty l <+> pretty f

instance Pretty M.PostingMeta where
  pretty (M.PostingMeta l mf) =
    text "Posting meta:" <+> pretty l
    <+> maybePretty "posting format" mf

instance Pretty M.TransactionMeta where
  pretty (M.TransactionMeta tml tl f) =
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

instance Pretty C.Item where
  pretty i = hang (text "Item") indent $ case i of
    (C.Transaction t) -> hang (text "Transaction") indent $ pretty t
    (C.Price pb) -> hang (text "Price box") indent $ pretty pb

instance Pretty C.Ledger where
  pretty (C.Ledger ls) =
    hang (text "Ledger") indent $
    vcat (map toDoc ls)
    where
      toDoc (l, i) = text "line no:" <+> pretty l
                     <+> text "item:" <+> pretty i
                         
