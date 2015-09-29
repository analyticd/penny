module Penny.Dump where

import Penny.Arrangement
import Penny.Decimal
import Penny.Natural
import Penny.Realm
import Penny.Scalar
import Penny.Serial
import Penny.Tree

import Data.Monoid ((<>))
import Data.Foldable (toList)
import Data.Text (unpack)
import Data.Time (formatTime, defaultTimeLocale)
import Text.PrettyPrint (Doc, text, (<+>), nest, vcat, ($$))

class Pretty a where
  pretty :: a -> Doc

instance Pretty Scalar where
  pretty x = text "scalar -" <+> case x of
    SText txt -> text "text -" <+> text (show txt)
    SDay day -> text "day -" <+> text (formatTime defaultTimeLocale "%F" day)
    STime tod -> text "time -" <+>
      text (formatTime defaultTimeLocale "%T" tod)
    SZone int -> text "zone -" <+> text (show int)
    SInteger int -> text "integer -" <+> text (show int)

instance Pretty Realm where
  pretty x = text "realm -" <+> text (show x)

indent :: Doc -> Doc
indent = nest 4

instance Pretty Tree where
  pretty tree = vcat
    [ text "tree"
    , indent (pretty . _realm $ tree)
    , indent (maybe (text "No scalar") pretty . _scalar $ tree)
    , indent (text "children" $$
                    (vcat . toList . fmap pretty . _children $ tree))
    ]

instance Pretty Unsigned where
  pretty = text . show . naturalToInteger

instance Pretty Serset where
  pretty (Serset fwd bak)
    = pretty fwd <> text ", " <> pretty bak

instance Pretty Serpack where
  pretty (Serpack file global)
    = text "File: " <> pretty file <> text " Global: " <> pretty global

instance Pretty a => Pretty (Exponential a) where
  pretty (Exponential coeff pow)
    = text "Coefficient: " <> pretty coeff <> text " Power: " <> pretty pow

instance Pretty Orient where pretty = text . show

instance Pretty Bool where pretty = text . show

instance Pretty Arrangement where
  pretty (Arrangement orient space)
    = text "Orientation: " <> pretty orient
    <> text " Space between: " <> pretty space
