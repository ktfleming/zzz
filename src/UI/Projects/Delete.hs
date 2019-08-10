{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Delete where

import           Types.AppState
import           Types.Models.Project

import           Control.Lens
import qualified Data.Text                     as T

deleteProject :: AppState -> ProjectContext -> AppState
deleteProject s (ProjectContext pid) = s & projects . at pid .~ Nothing

deleteProjectWarning :: AppState -> ProjectContext -> T.Text
deleteProjectWarning s c =
  let Project { _projectName } = lookupProject s c
  in  "Are you sure you want to delete project '"
        <> _projectName
        <> "'? All contained request definitions will also be deleted!"
