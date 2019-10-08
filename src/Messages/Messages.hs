{-# LANGUAGE RebindableSyntax #-}

module Messages.Messages where

import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , imodify
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( getCurrentTime )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState

logMessage :: MonadIO m => T.Text -> IxStateT m (AppState a) (AppState a) ()
logMessage msg = do
  time <- liftIO getCurrentTime
  let fullMessage = Message { messageDateTime = time, messageText = msg }
  imodify $ messages %~ (|> fullMessage)
