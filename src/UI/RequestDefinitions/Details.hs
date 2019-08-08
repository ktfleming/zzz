module UI.RequestDefinitions.Details where

import           Types.Models.RequestDefinition (RequestDefinitionContext)

import           Lens.Micro.Platform            ((&), (.~))
import           Types.AppState
import           Types.Models.Screen

showRequestDefinitionDetails :: AppState -> RequestDefinitionContext -> AppState
showRequestDefinitionDetails s c = s & activeScreen .~ RequestDetailsScreen c
