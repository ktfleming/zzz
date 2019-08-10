{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefinitions.Delete where

import           Control.Lens
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Models.Project           ( requestDefinitions )
import           Types.Models.RequestDefinition

deleteRequestDefinition :: AppState -> RequestDefinitionContext -> AppState
deleteRequestDefinition s (RequestDefinitionContext pid rid) =
  s & projects . at pid . _Just . requestDefinitions . at rid .~ Nothing

deleteRequestDefinitionWarning :: AppState -> RequestDefinitionContext -> T.Text
deleteRequestDefinitionWarning s c =
  let r = lookupRequestDefinition s c
  in  "Are you sure you want to delete request definition '"
        <> r
        ^. name
        .  coerced
        <> "'?"
