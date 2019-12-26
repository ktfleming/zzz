module Messages.Messages where

import Control.Monad.IO.Class
import Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Types.Constants

logMessage :: MonadIO m => T.Text -> m ()
logMessage msg = do
  time <- liftIO getCurrentTime
  let fullMessage = (show time) <> (T.unpack msg) <> "\n" -- TODO: better formatting for time, customizable timezone
  liftIO $ appendFile logFile fullMessage
