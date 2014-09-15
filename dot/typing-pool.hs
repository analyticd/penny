{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Typist.Typedesc as Td
import qualified Typist.Typename as Ty
import qualified Typist.NoVariables as Nov
import qualified Typist.Constructor as Ctor
import Prelude hiding
  ( maybe
  , either
  , seq
  )

anna :: Ty.T -> Td.T
anna ty = Td.T (Ty.T "Penny.Anna.T" [ty]) ctors
  where
    ctors = [ Ctor.T "Nil" [(Ty.T "Penny.Nil.T" [ty])]
            , Ctor.T "Brim" [(Ty.T "Penny.Brim.T" [ty])]
            ]

bg1 :: Ty.T -> Td.T
bg1 ty = Td.T (Ty.T "Penny.BG1.T" [ty]) ctors
  where
    ctors =
      [ Ctor.T "GroupOnLeft"
        [ ty, Ty.T "Penny.DecDecsMayGroups.T" [ty]
        , Ty.T "Prelude.Maybe" [Ty.T "Penny.BG2.T" [ty]]
        ]
      , Ctor.T "GroupOnRight"
        [ Ty.T "Penny.Radix.T" [ty]
        , Ty.T "Penny.DecDecsMayGroups.T" [ty]
        ]
      ]

bg2 :: Ty.T -> Td.T
bg2 ty = Td.product (Ty.T "Penny.BG2.T" [ty])
  [ Ty.T "Penny.Radix.T" [ty]
  , Ty.T "Prelude.Maybe" [Ty.T "Penny.DecDecsMayGroups.T" [ty]]
  ]

bg4 :: Ty.T -> Td.T
bg4 ty = Td.product (Ty.T "Penny.BG4.T" [ty])
  [ Ty.T "Prelude.Maybe" [Ty.T "Penny.Zero.T" []]
  , Ty.T "Penny.Radix.T" [ty]
  , Ty.T "Penny.BG5.T" [ty]
  ]

bg5 :: Ty.T -> Td.T
bg5 ty = Td.T (Ty.T "Penny.BG5.T" [ty])
  [ Ctor.T "Novem"
    [ Ty.T "Penny.NovSeqDecsNE.T" [ty] ]
  , Ctor.T "Zeroes"
    [ Ty.T "Penny.Zeroes.T" []
    , Ty.T "Penny.BG6.T" [ty]
    ]
  ]

bg6 :: Ty.T -> Td.T
bg6 ty = Td.T (Ty.T "Penny.BG6.T" [ty])
  [ Ctor.T "Novem"
    [ Ty.T "Penny.NovSeqDecsNE.T" [ty] ]
  , Ctor.T "Group"
    [ ty, Ty.T "Penny.BG7.T" [ty] ]
  ]

bg7 :: Ty.T -> Td.T
bg7 ty = Td.T (Ty.T "Penny.BG7.T" [ty])
  [ Ctor.T "LeadZeroes"
    [ Ty.T "Penny.Zeroes.T" []
    , Ty.T "Prelude.Either" [ty, Ty.T "Penny.BG7.T" [ty]]
    , Ty.T "Penny.Nodecs3.T" [ty]
    ]
  , Ctor.T "LeadNovem" [Ty.T "Penny.Nodecs3.T" [ty]]
  ]

bu2 :: Ty.T -> Td.T
bu2 ty = Td.T (Ty.T "Penny.BU2.T" [ty])
  [ Ctor.T "LeadingZero"
    [ Ty.T "Penny.Zerabu.T" [ty] ]
  , Ctor.T "NoLeadingZero"
    [ Ty.T "Penny.Radbu.T" [ty] ]
  ]


balanced :: Ty.T -> Td.T
balanced ty = Td.T (Ty.T "Penny.Balanced.T" [ty]) []

brim :: Ty.T -> Td.T
brim ty = Td.T (Ty.T "Penny.Brim.T" [ty])
  [ Ctor.T "Grouped" [Ty.T "Penny.BrimGrouped.T" [ty]]
  , Ctor.T "Ungrouped" [Ty.T "Penny.BrimUngrouped.T" [ty]]
  ]

brimGrouped :: Ty.T -> Td.T
brimGrouped ty = Td.T (Ty.T "Penny.BrimGrouped.T" [ty])
  [ Ctor.T "Masuno" [ Ty.T "Penny.NovDecs.T" []
                    , Ty.T "Penny.BG1.T" [ty] ]
  , Ctor.T "Fracuno" [Ty.T "Penny.BG4.T" [ty] ]
  ]


decDecsMayGroups :: Ty.T -> Td.T
decDecsMayGroups ty = Td.T (Ty.T "Penny.DecDecsMayGroups.T" [ty])
  [ Ctor.T "T" [ Ty.T "Penny.DecDecs.T" []
               , Ty.T "Penny.SeqDecs.T" [ty] ]
  ]

decsGroup :: Ty.T -> Td.T
decsGroup ty = Td.T (Ty.T "Penny.DecsGroup.T" [ty])
  [ Ctor.T "T" [ ty, Ty.T "Penny.DecDecs.T" [] ]]

ent :: Ty.T -> Td.T
ent ty = Td.T (Ty.T "Penny.Ent.T" [ty])
  [ Ctor.T "T" [ Ty.T "Penny.Qty.T" []
               , Ty.T "Penny.Commodity.T" []
               , ty
               ]
  ]

gravel :: Ty.T -> Td.T
gravel ty = Td.T (Ty.T "Penny.Gravel.T" [ty])
  [ Ctor.T "T"
    [ Ty.T "Prelude.Maybe"
      [ Ty.T "(,)" [ty, Ty.T "Penny.NovDecs.T" [] ] ]
    , Ty.T "Penny.Exp.T" []
    ]
  ]

grouper :: Ty.T -> Td.T
grouper ty = Td.T (Ty.T "Penny.Grouper.T" [ty])
  [ Ctor.T "Space" []
  , Ctor.T "Thin" []
  , Ctor.T "Under" []
  , Ctor.T "Unique" [ty]
  ]

nilUngrouped :: Ty.T -> Td.T
nilUngrouped ty = Td.T (Ty.T "Penny.NilUngrouped.T" [ty]) ctors
  where
    ctors = [ Ctor.T "LeadingZero" [(Ty.T "Penny.Znu1.T" [ty])]
            , Ctor.T "NoLeadingZero" [(Ty.T "Penny.RadZ.T" [ty])]
            ]

nodbu :: Ty.T -> Td.T
nodbu ty = Td.T (Ty.T "Penny.Nodbu.T" [ty])
  [ Ctor.T "T" [ Ty.T "Penny.NovDecs.T" []
               , Ty.T "Prelude.Maybe" [Ty.T "Penny.Radem.T" [ty]]
               ]
  ]

nodecs3 :: Ty.T -> Td.T
nodecs3 ty = Td.T (Ty.T "Penny.Nodecs3.T" [ty])
  [ Ctor.T "T" [ Ty.T "Penny.NovDecs.T" []
               , Ty.T "Penny.SeqDecs.T" [ty]
               ]
  ]

novSeqDecsNE :: Ty.T -> Td.T
novSeqDecsNE ty = Td.T (Ty.T "Penny.NovSeqDecsNE.T" [ty])
  [ Ctor.T "T" [ Ty.T "Penny.NovDecs.T" []
               , Ty.T "Penny.SeqDecsNE.T" [ty] ]
  ]

maybe :: Ty.T -> Td.T
maybe t1 = Td.T (Ty.T "Prelude.Maybe" [t1]) ctors
  where
    ctors = [ Ctor.empty "Nothing"
            , Ctor.T "Just" [t1]
            ]

maybeRadun :: Ty.T -> Td.T
maybeRadun ty = maybe $ Ty.T "Penny.Radun.T" [ty]

radix :: Ty.T -> Td.T
radix ty = Td.T (Ty.T "Penny.Radix.T" [ty]) ctors
  where
    ctors = [ Ctor.empty "T" ]

either :: Ty.T -> Ty.T -> Td.T
either t1 t2 = Td.T (Ty.T "Prelude.Either" [t1, t2]) ctors
  where
    ctors = [ Ctor.T "Left" [t1]
            , Ctor.T "Right" [t2] ]

ng1 :: Ty.T -> Td.T
ng1 ty = Td.product (Ty.T "Penny.NG1.T" [ty])
  [ Ty.T "Penny.Radix.T" [ty]
  , Ty.T "Penny.Zeroes.T" []
  , ty
  , Ty.T "Penny.Zeroes.T" []
  , Ty.T "Data.Sequence.Seq"
    [ Ty.T "Penny.ZGroup.T" [ty] ]
  ]

nil :: Ty.T -> Td.T
nil ty = Td.T (Ty.T "Penny.Nil.T" [ty]) cs
  where
    cs = [ Ctor.T "Ungrouped" [Ty.T "Penny.NilUngrouped.T" [ty]]
         , Ctor.T "Grouped" [Ty.T "Penny.NilGrouped.T" [ty]]
         ]

nilGrouped :: Ty.T -> Td.T
nilGrouped ty = Td.T (Ty.T "Penny.NilGrouped.T" [ty]) cs
  where
    cs = [ Ctor.T "LeadingZero" [Ty.T "Penny.Zng.T" [ty]]
         , Ctor.T "NoLeadingZero" [Ty.T "Penny.NG1.T" [ty]]
         ]

polarity :: Ty.T -> Ty.T -> Ty.T -> Td.T
polarity n o p = Td.T (Ty.T "Penny.Polarity.T" [n, o, p])
  [ Ctor.T "Center" [n]
  , Ctor.T "OffCenter" [o, p]
  ]

quant :: Ty.T -> Td.T
quant t = Td.T (Ty.T "Penny.Quant.T" [t])
  [ Ctor.T "T" [ Ty.T "Penny.NovDecs.T" []
               , Ty.T "Penny.Exp.T" []
               , t
               ]
  ]

radbu :: Ty.T -> Td.T
radbu t = Td.T (Ty.T "Penny.Radbu.T" [t])
  [ Ctor.T "T" [ Ty.T "Penny.Radix.T" [t]
               , Ty.T "Penny.BU3.T" []
               ]
  ]

radem :: Ty.T -> Td.T
radem t = Td.T (Ty.T "Penny.Radem.T" [t])
  [ Ctor.T "T" [ Ty.T "Penny.Radix.T" [t]
               , Ty.T "Penny.Decems.T" []
               ]
  ]

radZ :: Ty.T -> Td.T
radZ ty = Td.T (Ty.T "Penny.RadZ.T" [ty]) ctors
  where
    ctors = [ Ctor.T "T" [ Ty.T "Penny.Radix.T" [ty]
                         , Ty.T "Penny.Zeroes.T" [] ]]

radCom :: Ty.T
radCom = Ty.T "Penny.RadCom.T" []

radPer :: Ty.T
radPer = Ty.T "Penny.RadPer.T" []

radun :: Ty.T -> Td.T
radun ty = Td.T (Ty.T "Penny.Radun.T" [ty]) ctors
  where
    ctors = [ Ctor.T "T" [ Ty.T "Penny.Radix.T" [ty]
                         , Ty.T "Prelude.Maybe"
                                [Ty.T "Penny.Zeroes.T" []] ]]

seq_ZGroup :: Ty.T -> Td.T
seq_ZGroup ty =
  Td.T (Ty.T "Data.Sequence.Seq" [Ty.T "Penny.ZGroup.T" [ty]]) []

seqDecs :: Ty.T -> Td.T
seqDecs ty = Td.T (Ty.T "Penny.SeqDecs.T" [ty])
  [ Ctor.T "T" [ Ty.T "Data.Sequence.Seq"
                 [ Ty.T "Penny.DecsGroup.T" [ty] ]
               ]
  ]

seqDecsNE :: Ty.T -> Td.T
seqDecsNE ty = Td.T (Ty.T "Penny.SeqDecsNE.T" [ty])
  [ Ctor.T "T" [ Ty.T "Penny.DecsGroup.T" [ty]
               , Ty.T "Penny.SeqDecs.T" [ty]
               ]
  ]

znu1 :: Ty.T -> Td.T
znu1 ty = Td.T (Ty.T "Penny.Znu1.T" [ty]) [(Ctor.T "T" ctors)]
  where
    ctors = [ Ty.T "Penny.Zero.T" []
            , Ty.T "Prelude.Maybe" [Ty.T "Penny.Radun.T" [ty]]
            ]

zng :: Ty.T -> Td.T
zng ty = Td.T (Ty.T "Penny.Zng.T" [ty]) [(Ctor.T "T" ctors)]
  where
    ctors = [ Ty.T "Penny.Zero.T" []
            , Ty.T "Penny.NG1.T" [ty] ]



types :: [Td.T]
types =
  [
  -- Seq Decem
    Td.abstract (Ty.T "Data.Sequence.Seq"
                [Ty.T "Deka.Native.Abstract.Decem" []])

  , Nov.nullary "Deka.Dec.Sign"
    [ Ctor.empty "Sign0"
    , Ctor.empty "Sign1"
    ]

  , Nov.nullary "Deka.Native.Abstract.Decem"
    [ Ctor.T "D0" []
    , Ctor.T "Nonem" [Ty.T "Deka.Native.Abstract.Novem" []]
    ]

  , Nov.nullary "Deka.Native.Abstract.Novem" . map Ctor.empty
    . map ('D':)
    . map show $ [1 .. 9 :: Int]

  -- Begin Penny

  , Nov.wrapper "Penny.Account.T"
      (Ty.T "Data.Sequence.Seq" [Ty.T "Penny.SubAccount.T" []])

  , Nov.unit "Penny.Ampersand.T"
  , anna radCom
  , anna radPer
  , Nov.unit "Penny.Apostrophe.T"
  , Nov.wrapper "Penny.ArithmeticError"
      (Ty.T "Prelude.String" [])
  , Nov.product "Penny.Arrangement.T"
      [ Ty.noParams "Penny.Orient.T"
      , Ty.noParams "Penny.SpaceBetween.T"
      ]
  , Nov.unit "Penny.Asterisk.T"
  , Nov.unit "Penny.AtSign.T"

  , bg1 radCom
  , bg1 radPer

  , bg2 radCom
  , bg2 radPer

  , bg4 radCom
  , bg4 radPer

  , bg5 radCom
  , bg5 radPer

  , bg6 radCom
  , bg6 radPer

  , bg7 radCom
  , bg7 radPer

  , bu2 radCom
  , bu2 radPer

  , Nov.nullary "Penny.BU3.T"
    [ Ctor.T "Zeroes"
      [ Ty.T "Penny.Zenod.T" [] ]
    , Ctor.T "NoZeroes"
      [ Ty.T "Penny.NovDecs.T" [] ]
    ]

  , balanced (Ty.T "Penny.Posting.T" [])

  , Nov.wrapper "Penny.Balances.T"
    (Ty.T "Data.Map.Map" [ Ty.T "Penny.Commodity.T" []
                         , Ty.T "Penny.Qty.T" [] ])

  , Nov.wrapper "Penny.Bar.T" (Ty.T "Data.Text.Text" [])
  , Nov.unit "Penny.Brace.Close.T"
  , Nov.unit "Penny.Brace.Open.T"

  , brim radPer
  , brim radCom

  , brimGrouped radPer
  , brimGrouped radCom

  , Nov.product "Penny.Bundle.T"
    [ Ty.T "Penny.TopLine.T" []
    , Ty.T "Penny.View.T" [Ty.T "Penny.Posting.T" []]
    ]

  , Nov.unit "Penny.Caret.T"

  , Nov.product "Penny.Cement.T"
    [ Ty.T "Penny.Coeff.T" []
    , Ty.T "Penny.Exp.T" []
    ]

  , Nov.abstract "Penny.Char.Commodity.T"
  , Nov.wrapper "Penny.Clxn.T" $ Ty.T "Data.Text.Text" []

  , Nov.nullary "Penny.Coeff.T"
    [ Ctor.empty "Zero"
    , Ctor.T "NonZero" [Ty.T "Penny.NovSign.T" []]
    ]

  , Nov.product "Penny.CoefficientSign.T"
    [ Ty.T "Deka.Native.Abstract.Coefficient" []
    , Ty.T "Deka.Dec.Sign" []
    ]

  , Nov.unit "Penny.Colon.T"

  , Nov.wrapper "Penny.Commodity.T" $ Ty.T "Data.Text.Text" []

  , Nov.abstract "Penny.Concrete.T"
  , Nov.wrapper "Penny.Decems.T" (Ty.T "Data.Sequence.Seq"
                [Ty.T "Deka.Native.Abstract.Decem" []])

  , Nov.product "Penny.DateTime.T"
    [ Ty.T "Data.Time.Calendar.Day" []
    , Ty.T "Penny.Hours.T" []
    , Ty.T "Penny.Minutes.T" []
    , Ty.T "Penny.Seconds.T" []
    , Ty.T "Penny.TimeZoneOffset.T" []
    ]

  , Nov.product "Penny.DecDecs.T"
    [ Ty.T "Deka.Native.Abstract.Decem" []
    , Ty.T "Penny.Decems.T" []
    ]

  , Nov.wrapper "Penny.Decems.T" $
    Ty.T "Data.Sequence.Seq" [Ty.T "Deka.Native.Abstract.Decem" []]

  , decDecsMayGroups radPer
  , decDecsMayGroups radCom

  , decsGroup radPer
  , decsGroup radCom

  , Nov.unit "Penny.EOF"

  , ent (Ty.T "Penny.Posting.T" [])

  , Nov.nullary "Penny.EntCode.T" . map (\s -> Ctor.T s []) $
    [ "SCWrongSide", "SWrongSide", "CommodityNotFound",
      "NoCommoditiesInBalance", "MultipleCommoditiesInBalance",
      "QQtyTooBig" ]

  , Nov.product "Penny.EntError.T"
    [ Ty.T "Penny.EntCode.T" []
    , Ty.T "Penny.Trio.T" []
    , Ty.T "Penny.Imbalances.T" []
    ]

  , Nov.abstract "Penny.Ents.T"
  , Nov.wrapper "Penny.Exchange.T" $ Ty.T "Penny.Concrete.T" []

  , Nov.nullary "Penny.Exp.T"
    [ Ctor.empty "Zero"
    , Ctor.T "Negative" [Ty.T "Penny.NovDecs.T" []]
    ]

  , Nov.wrapper "Penny.Flag.T" $ Ty.T "Data.Text.Text" []

  , gravel $ Ty.T "Penny.Qty.T" []
  , gravel $ Ty.T "Penny.Exchange.T" []

  , grouper radCom
  , grouper radPer

  , Nov.abstract "Penny.Hours.T"
  , Nov.unit "Penny.Hyphen.T"
  , Nov.wrapper "Penny.Imbalances.T" $
    Ty.T "Data.Map.Map" [ Ty.T "Penny.Commodity.T" []
                        , Ty.T "Penny.Quark.T" [] ]
  , Nov.wrapper "Penny.Location.T" $ Ty.T "Prelude.Int" []

  , Nov.wrapper "Penny.Memo.T" $ Ty.T "Data.Sequence.Seq"
    [ Ty.T "Penny.Bar.T" [] ]

  , Nov.abstract "Penny.Minutes.T"

  , ng1 radCom
  , ng1 radPer

  , Nov.unit "Penny.Newline.T"

  , nil radCom
  , nil radPer

  , nilGrouped radCom
  , nilGrouped radPer

  , nilUngrouped radCom
  , nilUngrouped radPer

  , nodbu radCom
  , nodbu radPer

  , nodecs3 radCom
  , nodecs3 radPer

  , Nov.abstract "Penny.NonZero.T"

  , Nov.product "Penny.NovDecs.T"
    [ Ty.T "Deka.Native.Abstract.Novem" []
    , Ty.T "Penny.Decems.T" []
    ]

  , novSeqDecsNE radCom
  , novSeqDecsNE radPer

  , Nov.product "Penny.NovSign.T"
    [ Ty.T "Penny.NovDecs.T" []
    , Ty.T "Deka.Dec.Sign" []
    ]

  , Nov.wrapper "Penny.Number.T" $ Ty.T "Data.Text.Text" []
  , Nov.unit "Penny.Octothorpe.T"
  , Nov.nullary "Penny.Orient.T"
    [ Ctor.T "CommodityOnLeft" []
    , Ctor.T "CommodityOnRight" []
    ]
  , Nov.unit "Penny.Paren.Close.T"
  , Nov.unit "Penny.Paren.Open.T"
  , Nov.wrapper "Penny.Payee.T" $ Ty.T "Data.Text.Text" []
  , Nov.wrapper "Penny.Pebble.T" $ Ty.T "Penny.Gravel.T"
    [ Ty.T "Penny.Side.T" [] ]
  , Nov.nullary "Penny.PluMin.T"
    [ Ctor.T "Plus" [], Ctor.T "Minus" [] ]
  , Nov.unit "Penny.Plus.T"

  , Nov.product "Penny.Posting.T"
    [ Ty.T "Penny.Memo.T" []
    , Ty.T "Prelude.Maybe" [Ty.T "Penny.Number.T" []]
    , Ty.T "Prelude.Maybe" [Ty.T "Penny.Flag.T" []]
    , Ty.T "Prelude.Maybe" [Ty.T "Penny.Payee.T" []]
    , Ty.T "Penny.Tags.T" []
    , Ty.T "Penny.Account.T" []
    , Ty.T "Penny.Location.T" []
    , Ty.T "Penny.Serial.T" []
    , Ty.T "Penny.Serial.T" []
    , Ty.T "Penny.Trio.T" []
    ]

  , Nov.wrapper "Penny.Qty.T" (Ty.T "Penny.Concrete.T" [])

  , quant (Ty.T "Penny.Side.T" [])
  , quant (Ty.T "Penny.PluMin.T" [])

  , Nov.wrapper "Penny.Quark.T" $ Ty.T "Penny.Quant.T"
    [ Ty.T "Penny.Side.T" [] ]

  , Nov.nullary "Penny.RadCom.T"
    [ Ctor.T "T" [] ]

  -- Radix RadCom
  , radix (Ty.T "Penny.RadCom.T" [])

  , Nov.nullary "Penny.RadPer.T"
    [ Ctor.T "T" [] ]

  -- Radix RadPer
  , radix (Ty.T "Penny.RadPer.T" [])

  -- Radun
  , radun radCom
  , radun radPer

  , radbu radCom
  , radbu radPer

  , radem radCom
  , radem radPer

  , radZ radCom
  , radZ radPer

  , Nov.abstract "Penny.Seconds.T"
  , Nov.unit "Penny.Semicolon.T"

  , seqDecs radCom
  , seqDecs radPer

  , seqDecsNE radCom
  , seqDecsNE radPer

  , seq_ZGroup radCom
  , seq_ZGroup radPer

  , Nov.product "Penny.Serial.T"
    [ Ty.T "Prelude.Int" []
    , Ty.T "Prelude.Int" []
    ]

  , Nov.nullary "Penny.Side.T"
    [ Ctor.T "Debit" [], Ctor.T "Credit" [] ]

  -- START HERE - signed

  , Nov.nullary "Penny.Zero.T" [Ctor.empty "T"]
  , Nov.wrapper "Penny.Zeroes.T" (Ty.T "Penny.NonZero.T" [])

  , zng radCom
  , zng radPer

  , znu1 radCom
  , znu1 radPer

  -- End Penny


  , Nov.nullary "Prelude.Bool"
    [ Ctor.empty "True"
    , Ctor.empty "False"
    ]

  , Nov.abstract "Prelude.Int"

  -- Prelude.Maybe Penny.Radun.T
  , maybeRadun radCom
  , maybeRadun radPer

  -- Prelude.Maybe Penny.Zeroes.T
  , maybe (Ty.T "Penny.Zeroes.T" [])

  -- Maybe Int
  , maybe (Ty.T "Prelude.Int" [])

  -- Maybe Bool
  , maybe (Ty.T "Prelude.Bool" [])

  -- Either Int Bool
  , either (Ty.T "Prelude.Int" [])
           (Ty.T "Prelude.Bool" [])

  ]

main :: IO ()
main = putStr . Td.dotifyList $ types
