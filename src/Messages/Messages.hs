{-# LANGUAGE RebindableSyntax #-}

module Messages.Messages where

import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )

import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , imodify
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Types.AppState

logMessage :: MonadIO m => T.Text -> IxStateT m (AppState a) (AppState a) ()
logMessage m = do
  currentTime <- liftIO getCurrentTime
  let fullMessage =
        Message $ T.pack $ formatISO8601 currentTime <> ": " <> T.unpack m
  imodify $ messages %~ (|> fullMessage)
