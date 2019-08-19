{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Utils.IxState where

import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , imodify
                                                , iput
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

