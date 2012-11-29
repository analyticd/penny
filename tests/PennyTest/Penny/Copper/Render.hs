-- | Tests renderers by using the generators written for the
-- parser. Generates an item, then renders it, then parses it, and
-- tests to see whether the parsed item is the same as what was
-- generated.
module PennyTest.Penny.Copper.Render where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Copper.Render as R
import qualified PennyTest.Penny.Copper.Gen.Parsers as TP
import Penny.Copper.Parsec as P
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.Framework as TF
import Text.Parsec.Text (Parser)
import qualified Text.Parsec as Parsec
import qualified Data.Text as X
import qualified Test.QuickCheck.Property as QP

pTestByT
  :: (Show a, Show b)

  -> (a -> b -> Bool)
  -- ^ Tests what is parsed and what is rendered for equality

  -> String
  -- ^ Test description

  -> Parser a
  -- ^ Parses what is rendered

  -> TP.GenT (b, c)
  -- ^ Generates what is rendered. Also generates a rendering,
  -- which is ignored.

  -> (b -> Maybe X.Text)
  -- ^ Rendering function being tested

  -> TF.Test
pTestByT testEq desc parser gen rend = testProperty desc prop
  where
    prop = Ex.resolveT return $ do
      (toRender, _) <- gen
      rendered <- case rend toRender of
        Nothing -> Ex.throwT QP.failed { QP.reason = r }
          where r = "could not render text. "
                    ++ "Item that failed to render: " ++ show toRender
        Just r -> return r
      parsed <-
        case Parsec.parse (parser <* Parsec.eof) "" rendered of
          Left e -> Ex.throwT QP.failed { QP.reason = r }
            where r = "could not parse text. "
                      ++ "Item that was rendered: " ++ show toRender
                      ++ " Text that failed to parse: " ++ show rendered
                      ++ " Parse error message: " ++ show e
          Right g -> return g
      if testEq parsed toRender
      then return QP.succeeded
      else return QP.failed { QP.reason = r }
        where r = "parsed item not equal to original item."
                  ++ " Item that was rendered: " ++ show toRender
                  ++ " Text that was parsed: " ++ show rendered
                  ++ " Parse result: " ++ show parsed
