{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Monads where

import Brick (EventM)
import Brick.BChan
  ( BChan,
    writeBChan,
  )
import Control.Lens
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Singletons
  ( SingI,
    sing,
  )
import Types.AppState
  ( AnyAppState (..),
    AppState,
    screen,
  )
import Types.Brick.CustomEvent (CustomEvent (..))
import Types.Brick.Name (Name)
import Types.Classes.Fields
import Types.Models.Screen
  ( AnyScreen (..),
  )

newtype AppM a
  = AppM
      { runAppM :: (EventM Name) a
      }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Analogous to MonadIO, describing a monad into which Brick's EventM monad can be lifted.
-- The provided `a` is only for use in testing (see TestM)
class MonadIO m => MonadEvent m where
  liftEvent :: a -> EventM Name a -> m a

instance MonadEvent AppM where
  liftEvent _ = AppM

-- Wraps the tagged output state into an AnyAppState. This is necessary for
-- interacting with Brick's top-level event handler.
wrap :: SingI a => AppState a -> AnyAppState
wrap = AnyAppState sing

-- Send a custom event to Brick's event loop
sendEvent :: MonadIO m => CustomEvent -> BChan CustomEvent -> m ()
sendEvent ev chan = liftIO (writeBChan chan ev)

-- Run and get the result of the given monadic action (in practice, an updated
-- AppState), then fire a save event so that the updated state is saved. Finally
-- return the result.
saveAfter :: MonadIO m => BChan CustomEvent -> m a -> m a
saveAfter chan action = do
  result <- action
  sendEvent Save chan
  pure result

-- Store the currently displayed Screen in the AppState's `stashedScreen`, so
-- it can be restored with `unstashScreen`
stashScreen :: SingI a => AppState a -> AppState a
stashScreen s = s & stashedScreen ?~ AnyScreen sing (s ^. screen)

-- Remove the currently stashed screen (if there is one) and set it as the
-- currently displayed screen. Then set the currently stashed screen to nothing.
unstashScreen :: SingI a => AppState a -> AnyAppState
unstashScreen s = case s ^. stashedScreen of
  Nothing -> wrap s
  Just (AnyScreen tag stashed) -> AnyAppState tag $ s & (screen .~ stashed) . (stashedScreen .~ Nothing)
