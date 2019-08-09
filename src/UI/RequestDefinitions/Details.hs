{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.RequestDefinitions.Details where

import           Types.Models.RequestDefinition ( RequestDefinition(..)
                                                , RequestDefinitionContext
                                                , _requestDefinitionURL
                                                )

import           Brick                          ( Widget
                                                , txt
                                                )
import           Lens.Micro.Platform            ( (&)
                                                , (.~)
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name )
import           Types.Classes.Displayable      ( display )
import           Types.Models.Screen

showRequestDefinitionDetails :: AppState -> RequestDefinitionContext -> AppState
showRequestDefinitionDetails s c = s & activeScreen .~ RequestDetailsScreen c

requestDefinitionDetailsWidget
  :: AppState -> RequestDefinitionContext -> Widget Name
requestDefinitionDetailsWidget s c =
  let RequestDefinition { _requestDefinitionURL, _requestDefinitionMethod } =
          lookupRequestDefinition s c
      fullText =
          "Request: "
            <> display _requestDefinitionMethod
            <> " "
            <> _requestDefinitionURL
  in  txt fullText
