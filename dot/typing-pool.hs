module Main where

import qualified Typist.Typedesc as Td
import qualified Typist.Typename as Ty
import qualified Typist.NoVariables as Nov
import qualified Typist.Constructor as Ctor
import qualified Typist.Type1 as Type1
import qualified Typist.Kind1 as Kind1
import qualified Typist.Type2 as Type2
import qualified Typist.Kind2 as Kind2
import Prelude hiding
  ( maybe
  , either
  )

maybe :: Ty.T -> Td.T
maybe = Type1.toTypedesc $ Type1.T "Prelude.Maybe"
  [ Ctor.empty "Nothing"
  , Ctor.T "Just" [Kind1.Var0]
  ]

either :: Ty.T -> Ty.T -> Td.T
either = Type2.toTypedesc $ Type2.T "Prelude.Either"
  [ Ctor.T "Left" [Kind2.Var0]
  , Ctor.T "Right" [Kind2.Var1]
  ]

types :: [Td.T]
types =
  [ Nov.nullary "Deka.Native.Novem" . map Ctor.empty
    . map ('D':)
    . map show $ [1 .. 9 :: Int]

  , Nov.opaque "Penny.Concrete.T"
  , Nov.wrapper "Penny.Qty.T" (Ty.T "Penny.Concrete.T" [])

  , Nov.nullary "Prelude.Bool"
    [ Ctor.empty "True"
    , Ctor.empty "False"
    ]

  , Nov.opaque "Prelude.Int"

  -- Maybe Int
  , maybe (Ty.T "Prelude.Int" [])

  -- Maybe Bool
  , maybe (Ty.T "Prelude.Bool" [])

  -- Either Int Bool
  , either (Ty.T "Prelude.Int" []) (Ty.T "Prelude.Bool" [])
  ]

main :: IO ()
main = putStr . Td.dotifyList $ types
