module Penny.Lincoln.Pretty (
  Pretty(pretty)) where

import Data.Foldable (toList)
import Data.Text (unpack)
import Data.Time (formatTime)
import System.Locale (defaultTimeLocale)
import Text.PrettyPrint (
  punctuate, char, hcat, (<>), (<+>), text, Doc)

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty(TextNonEmpty(TextNonEmpty))

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
