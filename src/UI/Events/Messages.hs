{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.Events.Messages
  ( handleEventMessages,
  )
where

import Brick
  ( vScrollBy,
    viewportScroll,
  )
import Graphics.Vty.Input.Events
import Types.AppState
  ( AnyAppState (..),
    AppState,
  )
import Types.Brick.Name (Name (MessagesViewport))
import Types.Models.Screen
import Types.Monads

handleEventMessages ::
  MonadEvent m =>
  Key ->
  [Modifier] ->
  AppState 'MessagesTag ->
  m AnyAppState
handleEventMessages key _ s =
  let vp = viewportScroll MessagesViewport
      scrollAction = case key of
        KUp -> liftEvent () (vScrollBy vp (-5))
        KDown -> liftEvent () (vScrollBy vp 5)
        _ -> pure ()
   in scrollAction >> pure (wrap s)
