{-# LANGUAGE OverloadedStrings #-}

module UI.Title
  ( title
  )
where

import           Control.Lens
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Classes.Displayable      ( display )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen

requestDefinitionBaseTitle :: AppState -> RequestDefinitionContext -> T.Text
requestDefinitionBaseTitle s c@(RequestDefinitionContext pid _) =
  let p = lookupProject s (ProjectContext pid)
      r = lookupRequestDefinition s c
  in  p ^. name . coerced <> " > " <> r ^. name . coerced

title :: AppState -> T.Text
title s = case s ^. screen of
  ProjectAddScreen  _ -> "New Project"
  ProjectListScreen _ -> "All Projects"
  ProjectEditScreen c _ ->
    let p = lookupProject s c in p ^. name . coerced <> " (Editing)"
  ProjectDetailsScreen c _   -> let p = lookupProject s c in display p
  RequestAddScreen     _ _   -> "New Request Definition"
  RequestDetailsScreen c _ _ -> requestDefinitionBaseTitle s c
  RequestEditScreen c _      -> requestDefinitionBaseTitle s c <> " (Editing)"
  HelpScreen                 -> "Help"
  ConsoleScreen              -> "Messages"
