{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.StatusBar
  ( statusBar
  )
where

import           Brick                          ( Widget
                                                , fill
                                                , txt
                                                , vLimit
                                                , withAttr
                                                , (<+>)
                                                )
import           Control.Lens
import qualified Data.Text                     as T
import           Types.AppState                 ( AppState
                                                , currentEnvironment
                                                , screen
                                                )
import           Types.Brick.Name               ( Name )
import           Types.Classes.Displayable      ( display )
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Models.Environment       ( EnvironmentContext(..)
                                                , EnvironmentName(..)
                                                )
import           Types.Models.Project           ( ProjectContext(..) )
import           Types.Models.RequestDef        ( RequestDefContext(..) )
import           Types.Models.Screen
import           UI.Attr                        ( statusBarAttr )

statusBar :: AppState s -> Widget Name
statusBar s =
  let envName :: T.Text = maybe "No environment" (^. name . coerced) (currentEnvironment s)
  in  (withAttr statusBarAttr . vLimit 1) $ txt (title s) <+> fill ' ' <+> txt envName

projectBaseTitle :: AppState a -> ProjectContext -> T.Text
projectBaseTitle s c = display (model s c)

requestDefBaseTitle :: AppState a -> RequestDefContext -> T.Text
requestDefBaseTitle s c@(RequestDefContext pid _) =
  let p = model s (ProjectContext pid)
      r = model s c
  in  display p <> " > " <> display r

environmentBaseTitle :: AppState a -> EnvironmentContext -> T.Text
environmentBaseTitle s c = display (model s c)

title :: AppState a -> T.Text
title s = case s ^. screen of
  ProjectAddScreen  _           -> "New Project"
  ProjectListScreen _           -> "All Projects"
  ProjectDetailsScreen c _      -> projectBaseTitle s c
  ProjectEditScreen    c _      -> projectBaseTitle s c <> " (Editing)"
  RequestDefAddScreen  _ _      -> "New Request Definition"
  RequestDefDetailsScreen c _ _ -> requestDefBaseTitle s c
  RequestDefEditScreen c _      -> requestDefBaseTitle s c <> " (Editing)"
  EnvironmentListScreen{}       -> "Environments"
  EnvironmentEditScreen c _     -> environmentBaseTitle s c <> " (Editing)"
  EnvironmentAddScreen{}        -> "New Environment"
  HelpScreen                    -> "Help"
  SearchScreen{}                -> "Search"
  MessagesScreen                -> "Messages"
