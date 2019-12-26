{-# LANGUAGE FlexibleContexts #-}

module Messages.Messages where

import Control.Lens
import Control.Monad.IO.Class
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Types.AppState
import Types.Classes.Fields

logMessage :: (HasMessages a (Seq Message), MonadIO m) => T.Text -> a -> m a
logMessage msg s = do
  time <- liftIO getCurrentTime
  let fullMessage = Message {messageDateTime = time, messageText = msg}
  pure $ s & messages %~ (|> fullMessage)
