{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Delete where

import Control.Lens
import Data.Text (Text)
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.Project

deleteProject :: ProjectContext -> AppState a -> AppState a
deleteProject (ProjectContext pid) = projects . at pid .~ Nothing

deleteProjectWarning :: AppState a -> ProjectContext -> Text
deleteProjectWarning s c =
  let p = model s c
   in "Are you sure you want to delete project '"
        <> p
        ^. name
        . coerced
        <> "'? All contained request definitions will also be deleted!"
