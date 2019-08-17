module Messages.Messages where

import           Control.Lens
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.State.Lazy ( StateT
                                                , modify
                                                )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Types.AppState

logMessage :: MonadIO m => T.Text -> StateT AppState m ()
logMessage m = do
  currentTime <- liftIO getCurrentTime
  let fullMessage =
        Message $ T.pack $ formatISO8601 currentTime <> ": " <> T.unpack m
  modify $ messages %~ (|> fullMessage)
