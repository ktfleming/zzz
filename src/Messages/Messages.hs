module Messages.Messages where

import           Control.Lens
import qualified Data.Text                     as T
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Types.AppState

logMessage :: AppState -> T.Text -> IO AppState
logMessage s m = do
  currentTime <- getCurrentTime
  let fullMessage =
        Message $ T.pack $ formatISO8601 currentTime <> ": " <> T.unpack m
  return $ s & messages <>~ [fullMessage]
