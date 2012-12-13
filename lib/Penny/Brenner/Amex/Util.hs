module Penny.Brenner.Amex.Util where

import qualified Penny.Brenner.Amex.Types as Y
import qualified System.IO.Strict as Strict
import qualified System.IO.Error as IOE
import Text.Show.Pretty (ppShow)


-- | Loads the database from disk. If allowNew is True, then does not
-- fail if the file was not found.
loadDb
  :: Y.AllowNew
  -- ^ Is a new file allowed?

  -> Y.DbLocation
  -- ^ DB location

  -> IO Y.DbList
loadDb (Y.AllowNew allowNew) (Y.DbLocation dbLoc) = do
  eiStr <- IOE.tryIOError (Strict.readFile dbLoc)
  case eiStr of
    Left e ->
      if allowNew && IOE.isDoesNotExistError e
      then return []
      else IOE.ioError e
    Right g -> return . read $ g

-- | Writes a new database to disk.
saveDb :: Y.DbLocation -> Y.DbList -> IO ()
saveDb (Y.DbLocation p) =
  writeFile p
  .(++ "\n")
  . ppShow

