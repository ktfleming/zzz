{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.HelpPanel
  ( helpPanel
  )
where

import           Brick                          ( Widget
                                                , txt
                                                )
import qualified Data.Text                     as T
import           Types.Brick.Name               ( Name )
import           Types.Models.Screen

helpText :: Screen a -> T.Text
helpText s = case s of
  ProjectAddScreen{}  -> "Enter: Finish adding | ESC: Return without adding"
  ProjectListScreen{} -> "Enter: View project | a: Add Project"
  ProjectDetailsScreen{}
    -> "Enter: View request definition | Left: back | e: Edit Project | a: Add request definition | d: Delete"
  RequestDefDetailsScreen{} -> "Enter: Send request | Left: back | e: Edit | d: Delete"
  ProjectEditScreen{}       -> "Enter: Save | ESC: Return without saving"
  RequestDefAddScreen{}     -> "Enter: Finsh adding | ESC: Return without adding"
  RequestDefEditScreen{}    -> "Enter: Save | ESC: Return without saving"
  EnvironmentListScreen{}   -> "todo"
  EnvironmentEditScreen{}   -> "todo"
  EnvironmentAddScreen{}    -> "todo"
  HelpScreen                -> "todo"
  SearchScreen{}            -> "todo"
  MessagesScreen            -> "todo"

helpPanel :: Screen a -> Widget Name
helpPanel s = txt $ helpText s
