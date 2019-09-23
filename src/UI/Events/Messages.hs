{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module UI.Events.Messages
  ( handleEventMessages
  )
where


import           Brick                          ( EventM
                                                , vScrollBy
                                                , viewportScroll
                                                )
import           Control.Monad.Indexed.State    ( IxStateT )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Graphics.Vty.Input.Events
import           Types.AppState                 ( AnyAppState
                                                , AppState
                                                )
import           Types.Brick.Name               ( Name(MessagesViewport) )
import           Types.Models.Screen
import           Utils.IxState                  ( submerge
                                                , (>>>)
                                                )

handleEventMessages
  :: Key -> [Modifier] -> IxStateT (EventM Name) (AppState 'MessagesTag) AnyAppState ()
handleEventMessages key _ =
  let vp = viewportScroll MessagesViewport
  in  case key of
        KUp   -> ilift (vScrollBy vp (-5)) >>> submerge
        KDown -> ilift (vScrollBy vp 5) >>> submerge
        _     -> submerge
