{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}

module Utils.IxState where

import           Brick.BChan                    ( BChan
                                                , writeBChan
                                                )
import           Control.Lens
import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                , iput
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.CustomEvent        ( CustomEvent(..) )
import           Types.Models.Screen

-- Analogous to >> for regular monads
(>>>) :: Monad m => IxStateT m i j a -> IxStateT m j k b -> IxStateT m i k b
(>>>) f g = f >>>= const g

-- "Submerges" the tagged output state of an IxStateT into an AnyAppState
submerge :: Monad m => IxStateT m (AppState i) AnyAppState ()
submerge = imodify AnyAppState

-- Extract a tagged screen from a tagged AppState
extractScreen :: Monad m => IxStateT m (AppState i) (Screen i) ()
extractScreen = iget >>>= \s -> iput $ s ^. screen

-- Given a tagged AppState, update its screen to the tagged screen currently
-- in the state, and put the updated AppState in the state
wrapScreen :: Monad m => AppState i -> IxStateT m (Screen i) (AppState i) ()
wrapScreen s = iget >>>= \scr -> iput $ s & screen .~ scr

-- Given a tagged AppState and a function in IxStateT which has
--   input state: that tagged AppState (with the matching tag)
--   output state: AnyAppState
-- , run the function on the input state and return the appropriate IxStateT that can be used in `handleEventInState`
(|$|)
  :: Monad m
  => IxStateT m (AppState i) AnyAppState ()
  -> AppState i
  -> IxStateT m AnyAppState AnyAppState ()
(|$|) ixs i = iput i >>> ixs

-- Sends the 'save' event to Brick's event loop, inside the IxStateT stack
save :: MonadIO m => BChan CustomEvent -> IxStateT m i i ()
save = sendEvent Save

sendEvent :: MonadIO m => CustomEvent -> BChan CustomEvent -> IxStateT m i i ()
sendEvent ev chan = (ilift . liftIO) (writeBChan chan ev)

stashScreen :: Monad m => IxStateT m (AppState a) (AppState a) ()
stashScreen = do
  s <- iget
  imodify $ stashedScreen ?~ AnyScreen (s ^. screen)

unstashScreen :: Monad m => IxStateT m (AppState i) AnyAppState ()
unstashScreen = do
  s <- iget
  case s ^. stashedScreen of
    Nothing                  -> submerge
    Just (AnyScreen stashed) -> imodify $ AnyAppState . (screen .~ stashed)

