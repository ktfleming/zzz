{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans (ilift)
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
    Screen,
  )

-- The main monad that this app runs in. It uses the IxState indexed monad to
-- represent type-safe transitions between tagged AppStates, where the tag
-- defines which screen of the app the user is viewing / interacting with.
-- It includes Brick's EventM monad (which itself includes IO) to do event handling.
newtype AppM i o a
  = AppM
      { runAppM :: IxStateT (EventM Name) i o a
      }
  deriving (IxPointed, IxFunctor, IxApplicative, IxMonad, IxMonadState)

-- When the input and output states are the same type, can treat this as a regular monad
deriving instance Functor (AppM i i)

deriving instance Applicative (AppM i i)

deriving instance Monad (AppM i i)

deriving instance MonadIO (AppM i i)

-- Analogous to MonadIO, describing a indexed monad into which Brick's EventM monad can be lifted.
-- Note that since this is for indexed monads, `m` is of kind * -> * -> * -> * instead of * -> *.
-- The provided `a` is only for use in testing (see TestM)
class IxMonadEvent m where
  iliftEvent :: a -> EventM Name a -> m i i a

instance IxMonadEvent AppM where
  iliftEvent _ = AppM . ilift

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
submerge :: (SingI i, IxMonadState m) => m (AppState i) AnyAppState ()
submerge = imodify $ AnyAppState sing

-- A different way to call `submerge`, to be used as a prefix.
-- Note that `submerge` is always the last call in the chain, so we can just say
-- sm $ do
--   ...
-- The idea is to de-emphasize the `submerge` at the end of the chain, since it's not
-- really important and is only there to make the types work.
sm :: (SingI o, IxMonadState m) => m a (AppState o) () -> m a AnyAppState ()
sm f = f >>> submerge

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
(|$|) ::
  IxMonadState m => m (AppState i) AnyAppState () -> AppState i -> m AnyAppState AnyAppState ()
(|$|) ixs i = iput i >>> ixs

-- Send a custom event to Brick's event loop
sendEvent :: IxMonadIO m => CustomEvent -> BChan CustomEvent -> m i i ()
sendEvent ev chan = iliftIO (writeBChan chan ev)

-- Store the currently displayed Screen in the AppState's `stashedScreen`, so
-- it can be restored with `unstashScreen`
stashScreen :: (SingI a, IxMonadState m) => m (AppState a) (AppState a) ()
stashScreen = iget >>>= \s -> imodify $ stashedScreen ?~ AnyScreen sing (s ^. screen)

-- Remove the currently stashed screen (if there is one) and set it as the
-- currently displayed screen. Then set the currently stashed screen to nothing.
unstashScreen :: (SingI i, IxMonadState m) => m (AppState i) AnyAppState ()
unstashScreen = iget >>>= \s -> case s ^. stashedScreen of
  Nothing -> submerge
  Just (AnyScreen tag stashed) -> imodify $ AnyAppState tag . (screen .~ stashed) . (stashedScreen .~ Nothing)
