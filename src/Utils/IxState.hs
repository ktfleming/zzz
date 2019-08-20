{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RebindableSyntax #-}

module Utils.IxState where

import           Control.Lens
import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                , iput
                                                )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Models.Screen

-- Analogous to >> for regular monads
(>>>) :: Monad m => IxStateT m i j a -> IxStateT m j k b -> IxStateT m i k b
(>>>) f g = f >>>= const g

-- "Submerges" the tagged output state of an IxStateT into an AnyAppState
submerge :: Monad m => IxStateT m i (AppState (o :: ScreenTag)) () -> IxStateT m i AnyAppState ()
submerge ixs = ixs >>> imodify AnyAppState

-- Given a tagged AppState and a function in IxStateT which has
--   input state: that tagged AppState (with the matching tag)
--   output state: AnyAppState
-- , run the function on the input state and return the appropriate IxStateT that can be used in `handleEventInState`
(|$|)
  :: Monad m
  => IxStateT m (AppState (i :: ScreenTag)) AnyAppState ()
  -> AppState (i :: ScreenTag)
  -> IxStateT m AnyAppState AnyAppState ()
(|$|) ixs i = iput i >>> ixs


-- Given a Screen-level IxStateT function, extract the appropriately-typed Screen from the current
-- state, apply the function, then wrap the resulting screen into AnyAppState.
-- n.b. This is intended to be called from an IxStateT context where the input is a typed AppState and
-- the output is AnyAppState.
runOnScreen
  :: Monad m
  => IxStateT m (Screen i) (Screen (o :: ScreenTag)) ()
  -> IxStateT m (AppState i) AnyAppState ()

runOnScreen t = do
  s             <- iget               -- Extract the tagged AppState
  _             <- iput $ s ^. screen -- replace the state with the tagged Screen
  _             <- t                  -- Apply the function that uses/modifies that tagged Screen
  updatedScreen <- iget   -- Extract the modified tagged Screen
  iput $ AnyAppState (s & screen .~ updatedScreen) -- Wrap into AnyAppState and place in the state
