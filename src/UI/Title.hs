{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Title
  ( title
  )
where

import           Control.Lens
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Classes.Displayable      ( display )
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Screen

requestDefBaseTitle :: AppState a -> RequestDefContext -> T.Text
requestDefBaseTitle s c@(RequestDefContext pid _) =
  let p = model s (ProjectContext pid)
      r = model s c
  in  p ^. name . coerced <> " > " <> r ^. name . coerced

title :: AppState a -> T.Text
title s = case s ^. screen of
  ProjectAddScreen  _           -> "New Project"
  ProjectListScreen _           -> "All Projects"
  ProjectEditScreen    c _      -> let p = model s c in p ^. name . coerced <> " (Editing)"
  ProjectDetailsScreen c _      -> let p = model s c in display p
  RequestDefAddScreen  _ _      -> "New Request Definition"
  RequestDefDetailsScreen c _ _ -> requestDefBaseTitle s c
  RequestDefEditScreen c _      -> requestDefBaseTitle s c <> " (Editing)"
  EnvironmentListScreen{}       -> "Environments"
  --EnvironmentDetailsScreen{}    -> "Environment details"
  EnvironmentEditScreen{}       -> "Environment edit"
  EnvironmentAddScreen{}        -> "Environment add"
  HelpScreen                    -> "Help"
  SearchScreen{}                -> "Search"
