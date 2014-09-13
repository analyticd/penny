module Main where

import qualified Typist.Typedesc as Td
import qualified Typist.Typename as Ty
import qualified Typist.NoVariables as Nov
import qualified Typist.Constructor as Ctor
import qualified Typist.Type1 as Type1
import qualified Typist.Kind1 as Kind1
import qualified Typist.Type2 as Type2
import qualified Typist.Kind2 as Kind2
import qualified Typist.Identifier as Iden
import Prelude hiding
  ( maybe
  , either
  )

name :: String -> Iden.T
name = Iden.fromString

maybe :: Ty.T -> Td.T
maybe = Type1.toTypedesc $ Type1.T (name "Prelude.Maybe")
  [ Ctor.empty "Nothing"
  , Ctor.T "Just" [Kind1.Var0]
  ]

either :: Ty.T -> Ty.T -> Td.T
either = Type2.toTypedesc $ Type2.T (name "Prelude.Either")
  [ Ctor.T "Left" [Kind2.Var0]
  , Ctor.T "Right" [Kind2.Var1]
  ]

types :: [Td.T]
types =
  [ Nov.nullary (name "Deka.Native.Abstract.Decem")
    [ Ctor.T "D0" []
    , Ctor.T "Nonem" [Ty.T (name "Deka.Native.Abstract.Novem") []]
    ]

  , Nov.nullary (name "Deka.Native.Abstract.Novem") . map Ctor.empty
    . map ('D':)
    . map show $ [1 .. 9 :: Int]

  , Nov.opaque (name "Penny.Concrete.T")
  , Nov.wrapper (name "Penny.Qty.T") (Ty.T (name "Penny.Concrete.T") [])

  , Nov.nullary (name "Prelude.Bool")
    [ Ctor.empty "True"
    , Ctor.empty "False"
    ]

  , Nov.opaque (name "Prelude.Int")

  -- Maybe Int
  , maybe (Ty.T (name "Prelude.Int") [])

  -- Maybe Bool
  , maybe (Ty.T (name "Prelude.Bool") [])

  -- Either Int Bool
  , either (Ty.T (name "Prelude.Int") [])
           (Ty.T (name "Prelude.Bool") [])
  ]

main :: IO ()
main = putStr . Td.dotifyList $ types
