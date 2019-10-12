{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Types.Monads where

import           Types.Brick.Name               ( Name )

import           Brick                          ( EventM )

import           Brick.BChan                    ( BChan
                                                , writeBChan
                                                )
import           Control.Lens
import           Control.Monad.Indexed
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans    ( ilift )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Types.AppState                 ( AnyAppState(..)
                                                , AppState
                                                , screen
                                                , stashedScreen
                                                )
import           Types.Brick.CustomEvent        ( CustomEvent(..) )
import           Types.Models.Screen            ( AnyScreen(..)
                                                , Screen
                                                )

-- The main monad that this app runs in. It uses the IxState indexed monad to
-- represent type-safe transitions between tagged AppStates, where the tag
-- defines which screen of the app the user is viewing / interacting with.
-- It includes Brick's EventM monad (which itself includes IO) to do event handling.
newtype AppM i o a = AppM {
  runAppM :: IxStateT (EventM Name) i o a
} deriving (IxPointed, IxFunctor, IxApplicative, IxMonad, IxMonadState)

-- When the input and output states are the same type, can treat this as a regular monad
deriving instance Functor (AppM i i)
deriving instance Applicative (AppM i i)
deriving instance Monad (AppM i i)
deriving instance MonadIO (AppM i i)

-- Analogous to MonadIO, describing a indexed monad into which Brick's EventM monad can be lifted.
-- Note that since this is for indexed monads, `m` is of kind * -> * -> * -> * instead of * -> *
class IxMonadEvent m where
  iliftEvent :: EventM Name a -> m i i a

instance IxMonadEvent AppM where
  iliftEvent = AppM . ilift

-- Just a generalization of the regular MonadIO typeclass into one that works for indexed monads,
-- as long as the input and output types are the same (similar to IxMonadEvent)
class IxMonadIO m where
  iliftIO :: IO a -> m i i a

instance IxMonadIO AppM where
  iliftIO = liftIO

-- Analogous to >> for regular monads
(>>>) :: IxMonadState m => m i j a -> m j k b -> m i k b
(>>>) f g = f >>>= const g

-- "Submerges" the tagged output state into an AnyAppState. This is necessary for
-- interacting with Brick's top-level event handler.
submerge :: IxMonadState m => m (AppState i) AnyAppState ()
submerge = imodify AnyAppState

-- Extract a tagged screen from a tagged AppState
extractScreen :: IxMonadState m => m (AppState i) (Screen i) ()
extractScreen = iget >>>= \s -> iput $ s ^. screen

-- Given a tagged AppState, update its screen to the tagged screen currently
-- in the state, and put the updated AppState in the state
wrapScreen :: IxMonadState m => AppState i -> m (Screen i) (AppState i) ()
wrapScreen s = iget >>>= \scr -> iput $ s & screen .~ scr

-- Given a tagged AppState and a function in an indexed monad which has
--   input state: that tagged AppState (with the matching tag)
--   output state: AnyAppState
-- , run the function on the input state and return the appropriate monad that can be used in `handleEventInState`
(|$|)
  :: IxMonadState m => m (AppState i) AnyAppState () -> AppState i -> m AnyAppState AnyAppState ()
(|$|) ixs i = iput i >>> ixs

-- Send a custom event to Brick's event loop
sendEvent :: IxMonadIO m => CustomEvent -> BChan CustomEvent -> m i i ()
sendEvent ev chan = iliftIO (writeBChan chan ev)

-- Store the currently displayed Screen in the AppState's `stashedScreen`, so
-- it can be restored with `unstashScreen`
stashScreen :: IxMonadState m => m (AppState a) (AppState a) ()
stashScreen = iget >>>= \s -> imodify $ stashedScreen ?~ AnyScreen (s ^. screen)

-- Remove the currently stashed screen (if there is one) and set it as the
-- currently displayed screen
unstashScreen :: IxMonadState m => m (AppState i) AnyAppState ()
unstashScreen = iget >>>= \s -> case s ^. stashedScreen of
  Nothing                  -> submerge
  Just (AnyScreen stashed) -> imodify $ AnyAppState . (screen .~ stashed)

