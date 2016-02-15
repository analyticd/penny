{-# LANGUAGE FlexibleInstances #-}
module Penny.Dump where

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Clatch
import Penny.Decimal
import Penny.Grammar (BrimRadCom, BrimRadPer, NilRadCom, NilRadPer)
import Penny.Natural
import Penny.NonZero
import Penny.Polar
import Penny.Realm
import Penny.Report
import Penny.Scalar
import Penny.SeqUtil
import Penny.Serial
import Penny.Tree
import Penny.Troika

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (assocs)
import Data.Monoid ((<>))
import Data.Foldable (toList)
import Data.Text (Text, unpack, pack)
import Data.Time (formatTime, defaultTimeLocale)
import Pinchot (terminals)
import Rainbow (chunk)
import Text.PrettyPrint (Doc, text, nest, vcat, hang, sep,
  punctuate, brackets, render)

class Pretty a where
  pretty :: a -> Doc

label :: String -> Doc -> Doc
label lbl = hang (text (lbl ++ ":")) 2

instance Pretty Scalar where
  pretty x = case x of
    SText txt -> label "text" $ text (show txt)
    SDay day -> label "day" $ text (formatTime defaultTimeLocale "%F" day)
    STime tod -> label "time" $
      text (formatTime defaultTimeLocale "%T" tod)
    SZone int -> label "zone" $ text (show int)
    SInteger int -> label "integer" $ text (show int)

instance Pretty Realm where
  pretty x = text (show x)

indent :: Doc -> Doc
indent = nest 4

instance Pretty Tree where
  pretty tree = vcat
    [ label "realm" (pretty . _realm $ tree)
    , label "scalar" (pretty . _scalar $ tree)
    , label "children"
      (vcat . toList . fmap pretty . _children $ tree)
    ]

instance Pretty Unsigned where
  pretty = text . show . naturalToInteger

instance Pretty Serset where
  pretty (Serset fwd bak)
    = pretty fwd <> text ", " <> pretty bak

instance Pretty Serpack where
  pretty (Serpack file global)
    = sep [label "File" (pretty file), label "Global" (pretty global)]

instance Pretty a => Pretty (Exponential a) where
  pretty (Exponential coeff pow)
    = sep [label "Coefficient" $ pretty coeff, label "Power" $ pretty pow]

instance Pretty Orient where pretty = text . show

instance Pretty Bool where pretty = text . show

instance Pretty Arrangement where
  pretty (Arrangement orient space)
    = sep [ label "Orientation" $ pretty orient,
            label "Space between" $ pretty space ]

instance Pretty Pole where pretty = text . show

instance Pretty a => Pretty (Polarized a) where
  pretty (Polarized charged charge)
    = sep [ label "Charged" $ pretty charged
          , label "Charge" $ pretty charge
          ]

instance (Pretty n, Pretty o) => Pretty (Moderated n o) where
  pretty x = case x of
    Moderate n -> label "Moderate" $ pretty n
    Extreme plr -> label "Extreme" $ pretty plr

instance Pretty BrimRadCom where
  pretty = text . toList . terminals

instance Pretty BrimRadPer where
  pretty = text . toList . terminals

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = label "Left" $ pretty a
  pretty (Right a) = label "Right" $ pretty a

instance Pretty NilRadCom where
  pretty = text . toList . terminals

instance Pretty NilRadPer where
  pretty = text . toList . terminals

instance Pretty NonZero where
  pretty = text . show . nonZeroToInteger

instance Pretty Troiload where
  pretty x = case x of
    QC rep ar -> label "QC" $ sep [ pretty rep, pretty ar ]
    Q rep -> label "Q" $ pretty rep
    SC dec -> label "SC" $ pretty dec
    S dec -> label "S" $ pretty dec
    UC brim pole ar -> label "UC" $ sep [ pretty brim, pretty pole, pretty ar ]
    U brim pole -> label "U" $ sep [ pretty brim, pretty pole ]
    C dec -> label "C" $ pretty dec
    E dec -> label "E" $ pretty dec

instance Pretty Text where
  pretty = text . unpack

instance Pretty Integer where
  pretty = text . show

instance Pretty Troika where
  pretty (Troika commodity troiquant)
    = vcat [ label "commodity" $ pretty commodity
           , label "troiquant" $ pretty troiquant
           ]

instance Pretty Core where
  pretty (Core troika birth)
    = vcat [ label "troika" $ pretty troika
           , label "birth" $ pretty birth
           ]

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = sep [ label "fst" (pretty a), label "snd" (pretty b) ]

instance Pretty a => Pretty (Seq a) where
  pretty = brackets . sep . punctuate (text ",") . fmap pretty . toList

instance Pretty a => Pretty [a] where
  pretty = brackets . sep . punctuate (text ",") . fmap pretty

instance Pretty () where pretty _ = text "()"

instance Pretty a => Pretty (Slice a) where
  pretty (Slice l s r) = sep
    [ label "onSlice" (pretty s), label "onLeft" (pretty l),
      label "onRight" (pretty r) ]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = text "Nothing"
  pretty (Just a) = label "Just" (pretty a)

instance Pretty Amount where
  pretty (Amount commodity qty)
    = sep [ label "Commodity" (pretty commodity)
          , label "Qty" (pretty qty)
          ]

instance Pretty Balance where
  pretty (Balance map)
    = pretty . assocs $ map

data Dump = Dump
  deriving Show

instance Report Dump where
  printReport _ _ _
    = Seq.singleton
    . chunk
    . pack
    . render
    . pretty
