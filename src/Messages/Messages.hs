{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}

module Messages.Messages where

import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    imodify,
  )
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Language.Haskell.DoNotation
import Types.AppState
import Types.Classes.Fields
import Types.Monads
  ( IxMonadIO,
    iliftIO,
  )
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

logMessage :: (IxMonadState m, HasMessages a (Seq Message), IxMonadIO m) => T.Text -> m a a ()
logMessage msg = do
  time <- iliftIO getCurrentTime
  let fullMessage = Message {messageDateTime = time, messageText = msg}
  imodify $ messages %~ (|> fullMessage)
