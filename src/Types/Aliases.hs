module Types.Aliases where

import           Brick                          ( BrickEvent
                                                , EventM
                                                )
import           Types.AppState                 ( AppState )
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name               ( Name )

import           Control.Monad.Trans.State      ( StateT )

type EventHandlerFunction
  = BrickEvent Name CustomEvent -> StateT AppState (EventM Name) ()
