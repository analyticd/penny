module Penny.Brenner.Amex.Util where

import qualified Penny.Brenner.Amex.Types as Y
import qualified System.IO.Strict as Strict
import qualified Data.Map as M
import qualified System.IO.Error as IOE
import Text.Show.Pretty (ppShow)


-- | Loads the database from disk. If allowNew is True, then does not
-- fail if the file was not found.
loadDb
  :: Bool
  -- ^ Is a new file allowed?

  -> Y.DbLocation
  -- ^ DB location

  -> IO Y.DbMap
loadDb (Y.AllowNew allowNew) (Y.DbLocation dbLoc) = do
  eiStr <- IOE.tryIOError (Strict.readFile dbLoc)
  str <- case eiStr of
    Left e ->
      if allowNew && IOE.isDoesNotExistError e
      then return M.empty
      else IOE.ioError e
    Right g ->
      let f (u, t) = (u, (t, Y.parseEntry t))
      in return . M.fromList . map f . read $ g

-- | Writes a new database to disk.
saveDb :: Y.DbLocation -> Y.DbMap -> IO ()
saveDb p =
  writeFile p
  . ppShow
  . map (\(u, (t, _)) -> (u, t))
  . M.toList


