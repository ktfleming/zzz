{-# LANGUAGE OverloadedStrings #-}
module UI.RequestDefinitions.Details where

import           Brick                          ( Widget
                                                , txt
                                                )
import           Control.Lens
import           Types.AppState
import           Types.Brick.Name               ( Name )
import           Types.Classes.Displayable      ( display )
import           Types.Models.RequestDefinition
import           Types.Models.Screen

showRequestDefinitionDetails :: AppState -> RequestDefinitionContext -> AppState
showRequestDefinitionDetails s c = s & screen .~ RequestDetailsScreen c

requestDefinitionDetailsWidget
  :: AppState -> RequestDefinitionContext -> Widget Name
requestDefinitionDetailsWidget s c =
  let r        = lookupRequestDefinition s c
      fullText = "Request: " <> display (r ^. method) <> " " <> (r ^. url)
  in  txt fullText
