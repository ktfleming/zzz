{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Delete where

import           Types.AppState
import           Types.Models.Project
import           Types.Models.RequestDefinition ( name )

import           Control.Lens
import qualified Data.Text                     as T

deleteProject :: AppState -> ProjectContext -> AppState
deleteProject s (ProjectContext pid) = s & projects . at pid .~ Nothing

deleteProjectWarning :: AppState -> ProjectContext -> T.Text
deleteProjectWarning s c =
  let p = lookupProject s c
  in  "Are you sure you want to delete project '"
        <> p
        ^. name
        .  coerced
        <> "'? All contained request definitions will also be deleted!"
