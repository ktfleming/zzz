{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module UI.Events.Messages
  ( handleEventMessages
  )
where


import           Brick                          ( vScrollBy
                                                , viewportScroll
                                                )
import           Control.Monad.Indexed.State    ( IxMonadState )
import           Graphics.Vty.Input.Events
import           Types.AppState                 ( AnyAppState
                                                , AppState
                                                )
import           Types.Brick.Name               ( Name(MessagesViewport) )
import           Types.Models.Screen
import           Types.Monads

handleEventMessages
  :: (IxMonadState m, IxMonadEvent m)
  => Key
  -> [Modifier]
  -> m (AppState 'MessagesTag) AnyAppState ()
handleEventMessages key _ =
  let vp = viewportScroll MessagesViewport
  in  case key of
        KUp   -> iliftEvent (vScrollBy vp (-5)) >>> submerge
        KDown -> iliftEvent (vScrollBy vp 5) >>> submerge
        _     -> submerge
