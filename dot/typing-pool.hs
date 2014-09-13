{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Typist.Typedesc as Td
import qualified Typist.Typename as Ty
import qualified Typist.NoVariables as Nov
import qualified Typist.Constructor as Ctor
import qualified Typist.Identifier as Iden
import Prelude hiding
  ( maybe
  , either
  , seq
  )

name :: String -> Iden.T
name = Iden.fromString

nilUngrouped :: Ty.T -> Td.T
nilUngrouped ty = Td.T (Ty.T "Penny.NilUngrouped.T" [ty]) ctors
  where
    ctors = [ Ctor.T "LeadingZero" [(Ty.T "Penny.Znu1.T" [ty])]
            , Ctor.T "NoLeadingZero" [(Ty.T "Penny.RadZ.T" [ty])]
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
ng1 ty = Td.T (Ty.T "Penny.NG1.T" [ty]) [Ctor.T "T" cs]
  where
    cs = [ Ty.T "Penny.Radix.T" [ty]
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
nilGrouped ty = Td.T (Ty.T "Penny.NilGrouped" [ty]) cs
  where
    cs = [ Ctor.T "LeadingZero" [Ty.T "Penny.Zng.T" [ty]]
         , Ctor.T "NoLeadingZero" [Ty.T "Penny.NG1.T" [ty]]
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

  , Nov.product "Penny.Cement.T"
    [ Ty.T "Penny.Coeff.T" []
    , Ty.T "Penny.Exp.T" []
    ]

  , Nov.nullary "Penny.Exp.T"
    [ Ctor.empty "Zero"
    , Ctor.T "Negative" [Ty.T "Penny.NovDecs.T" []]
    ]

  , Nov.nullary "Penny.Coeff.T"
    [ Ctor.empty "Zero"
    , Ctor.T "NonZero" [Ty.T "Penny.NovSign.T" []]
    ]

  , Nov.abstract "Penny.Concrete.T"
  , Nov.wrapper "Penny.Decems.T" (Ty.T "Data.Sequence.Seq"
                [Ty.T "Deka.Native.Abstract.Decem" []])

  , ng1 radCom
  , ng1 radPer

  , nil radCom
  , nil radPer

  , nilGrouped radCom
  , nilGrouped radPer

  , nilUngrouped radCom
  , nilUngrouped radPer

  , Nov.abstract "Penny.NonZero.T"

  , Nov.product "Penny.NovSign.T"
    [ Ty.T "Penny.NovDecs.T" []
    , Ty.T "Deka.Dec.Sign" []
    ]

  , Nov.product "Penny.NovDecs.T"
    [ Ty.T "Deka.Native.Abstract.Novem" []
    , Ty.T "Penny.Decems.T" []
    ]

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

  , Nov.wrapper "Penny.Qty.T" (Ty.T "Penny.Concrete.T" [])
  , Nov.wrapper "Penny.Zeroes.T" (Ty.T "Penny.NonZero.T" [])

  , radZ radCom
  , radZ radPer

  , seq_ZGroup radCom
  , seq_ZGroup radPer

  , Nov.nullary "Penny.Zero.T" [Ctor.empty "T"]

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