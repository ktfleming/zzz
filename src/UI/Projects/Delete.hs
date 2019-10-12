{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Delete where

import           Control.Lens
import           Control.Monad.Indexed.State    ( IxMonadState
                                                , imodify
                                                )
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Models.Project

deleteProject :: IxMonadState m => ProjectContext -> m (AppState a) (AppState a) ()
deleteProject (ProjectContext pid) = imodify $ projects . at pid .~ Nothing

deleteProjectWarning :: AppState a -> ProjectContext -> T.Text
deleteProjectWarning s c =
  let p = model s c
  in  "Are you sure you want to delete project '"
        <> p
        ^. name
        .  coerced
        <> "'? All contained request definitions will also be deleted!"
