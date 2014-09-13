module Main where

import qualified Typist.Typedesc as Td
import qualified Typist.Typename as Ty
import qualified Typist.NoVariables as Nov
import qualified Typist.Constructor as Ctor
import qualified Typist.Type1 as Type1
import qualified Typist.Kind1 as Kind1
import Prelude hiding
  ( maybe
  )

maybe :: Ty.T -> Td.T
maybe = Type1.toTypedesc $ Type1.T "Prelude.Maybe"
  [ Ctor.empty "Nothing"
  , Ctor.T "Just" [Kind1.Var0]
  ]

types :: [Td.T]
types =
  [ Nov.nullary "Deka.Native.Novem" . map Ctor.empty
    . map ('D':)
    . map show $ [1 .. 9 :: Int]

  , Nov.nullary "Prelude.Bool"
    [ Ctor.empty "True"
    , Ctor.empty "False"
    ]
  , Nov.opaque "Prelude.Int"

  -- Maybe Int
  , maybe (Ty.T "Prelude.Int" [])

  -- Maybe Bool
  , maybe (Ty.T "Prelude.Bool" [])
  ]

main :: IO ()
main = putStr . Td.dotifyList $ types
