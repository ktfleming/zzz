{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefinitions.Delete where

import qualified Data.Text                      as T
import           Lens.Micro.Platform            (at, (&), (.~), _Just)
import           Types.AppState
import           Types.Models.Project           (requestDefinitions)
import           Types.Models.RequestDefinition

deleteRequestDefinition :: AppState -> RequestDefinitionContext -> AppState
deleteRequestDefinition s (RequestDefinitionContext pid rid) =
  s & projects . at pid . _Just . requestDefinitions . at rid .~ Nothing

deleteRequestDefinitionWarning :: AppState -> RequestDefinitionContext -> T.Text
deleteRequestDefinitionWarning s c =
  let RequestDefinition { _requestDefinitionName } =
          lookupRequestDefinition s c
  in  "Are you sure you want to delete request definition '"
        <> _requestDefinitionName
        <> "'?"
