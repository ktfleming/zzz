{-# LANGUAGE FlexibleContexts #-}

module Messages.Messages where

import Config
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601
import Types.Constants

logMessage :: (MonadReader AppConfig m, MonadIO m) => Text -> m ()
logMessage msg = do
  time <- liftIO getCurrentTime
  config <- ask
  let zonedTime = utcToZonedTime (config ^. timeZone) time
      fullMessage = iso8601Show zonedTime <> " " <> T.unpack msg <> "\n"
  liftIO $ appendFile logFile fullMessage
