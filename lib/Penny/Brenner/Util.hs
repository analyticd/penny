module Penny.Brenner.Util where

import Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Brenner.Types as Y
import qualified Data.ByteString as BS
import qualified System.IO.Error as IOE
import qualified Data.Serialize as S
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Penny.Copper.Parsec as CP
import qualified Text.Parsec as P
import qualified Penny.Lincoln as L
import qualified System.Console.MultiArg as MA

-- | An option for where the user would like to send output.
output :: MA.OptSpec (X.Text -> IO ())
output = MA.OptSpec ["output"] "o" . MA.OneArg $ \s ->
  if s == "-"
    then TIO.putStr
    else TIO.writeFile s

-- | Given a list of output options, returns a single IO action to
-- write to all given files. If the list was empty, returns an IO
-- action that writes to standard output. Ensures that the argument
-- Text is fully evaluated before running the IO action.
processOutput :: [X.Text -> IO ()] -> X.Text -> IO ()
processOutput ls =
  if null ls
  then TIO.putStr
  else \x -> x `seq` sequence_ . map ($ x) $ ls

-- | Loads the database from disk. If allowNew is True, then does not
-- fail if the file was not found.
loadDb
  :: Y.AllowNew
  -- ^ Is a new file allowed?

  -> Y.DbLocation
  -- ^ DB location

  -> IO Y.DbList
loadDb (Y.AllowNew allowNew) (Y.DbLocation dbLoc) = do
  eiStr <- IOE.tryIOError (BS.readFile . X.unpack $ dbLoc)
  case eiStr of
    Left e ->
      if allowNew && IOE.isDoesNotExistError e
      then return []
      else IOE.ioError e
    Right g -> case readDbTuple g of
      Ex.Exception e -> fail e
      Ex.Success good -> return good

-- | File version. Increment this when anything in the file format
-- changes.
version :: Int
version = 0

brenner :: String
brenner = "penny.brenner"

readDbTuple
  :: BS.ByteString
  -> Ex.Exceptional String Y.DbList
readDbTuple bs = do
  (s, v, ls) <- Ex.fromEither $ S.decode bs
  Ex.assert "database file format not recognized." $ s == brenner
  Ex.assert "wrong database version." $ v == version
  return ls

saveDbTuple :: Y.DbList -> BS.ByteString
saveDbTuple ls = S.encode (brenner, version, ls)

-- | Writes a new database to disk.
saveDb :: Y.DbLocation -> Y.DbList -> IO ()
saveDb (Y.DbLocation p) = BS.writeFile (X.unpack p) . saveDbTuple

-- | Parses quantities from amounts. All amounts should be verified as
-- having only digits, optionally followed by a point and then more
-- digits. All these values should parse. So if there is a problem it
-- is a programmer error. Apply error.
parseQty :: Y.Amount -> L.Qty
parseQty a = case P.parse CP.quantity "" (Y.unAmount a) of
  Left e -> error $ "could not parse quantity from string: "
            ++ (X.unpack . Y.unAmount $ a) ++ ": " ++ show e
  Right g -> g

label :: String -> X.Text -> String
label s x = s ++ ": " ++ X.unpack x ++ "\n"

-- | Shows a Posting in human readable format.
showPosting :: Y.Posting -> String
showPosting (Y.Posting dt dc nc am py fd) =
  label "Date" (X.pack . show . Y.unDate $ dt)
  ++ label "Description" (Y.unDesc dc)
  ++ label "Type" (X.pack $ case nc of
                    Y.Increase -> "increase"
                    Y.Decrease -> "decrease")
  ++ label "Amount" (Y.unAmount am)
  ++ label "Payee" (Y.unPayee py)
  ++ label "Financial institution ID" (Y.unFitId fd)
  ++ "\n"

showDbPair :: (Y.UNumber, Y.Posting) -> String
showDbPair (Y.UNumber u, p) =
  label "U number" (X.pack . show $ u)
  ++ showPosting p
