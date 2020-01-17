{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.StatusBar
  ( statusBar,
  )
where

import Brick
import Control.Lens
import Data.Text (Text)
import Types.AppState
  ( AppState,
    currentEnvironment,
    screen,
  )
import Types.Brick.Name (Name)
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.Environment
  ( EnvironmentContext (..),
    EnvironmentName (..),
  )
import Types.Models.Project
  ( ProjectContext (..),
    ProjectName (..),
  )
import Types.Models.RequestDef
  ( RequestDefContext (..),
    RequestDefName (..),
  )
import Types.Models.Screen
import UI.Attr
  ( environmentNameAttr,
    statusBarAttr,
  )

statusBar :: AppState s -> Widget Name
statusBar s =
  let envName :: Text = maybe "No environment" (^. name . coerced) (currentEnvironment s)
   in (withAttr statusBarAttr . vLimit 1) $
        hBox
          [ txt (title s),
            fill ' ',
            withAttr environmentNameAttr (txt envName)
          ]

projectBaseTitle :: AppState a -> ProjectContext -> Text
projectBaseTitle s c = model s c ^. name . coerced

requestDefBaseTitle :: AppState a -> RequestDefContext -> Text
requestDefBaseTitle s c@(RequestDefContext pid _) =
  let p = model s (ProjectContext pid)
      r = model s c
   in p ^. name . coerced <> " > " <> r ^. name . coerced

environmentBaseTitle :: AppState a -> EnvironmentContext -> Text
environmentBaseTitle s c = model s c ^. name . coerced

title :: AppState a -> Text
title s = case s ^. screen of
  ProjectAddScreen _ -> "New Project"
  ProjectListScreen {} -> "All Projects"
  ProjectDetailsScreen c _ -> projectBaseTitle s c
  ProjectEditScreen c _ -> projectBaseTitle s c <> " (Editing)"
  RequestDefAddScreen _ _ -> "New Request Definition"
  RequestDefDetailsScreen c _ _ _ _ -> requestDefBaseTitle s c
  RequestDefEditScreen c _ -> requestDefBaseTitle s c <> " (Editing)"
  EnvironmentListScreen {} -> "Environments"
  EnvironmentEditScreen c _ -> environmentBaseTitle s c <> " (Editing)"
  EnvironmentAddScreen {} -> "New Environment"
  HelpScreen -> "Help"
  SearchScreen {} -> "Search"
