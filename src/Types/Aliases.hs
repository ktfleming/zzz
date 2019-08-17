module Types.Aliases where

import           Brick                          ( BrickEvent
                                                , EventM
                                                , Next
                                                )
import           Types.AppState                 ( AppState )
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name               ( Name )

type EventHandlerFunction
  = AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
