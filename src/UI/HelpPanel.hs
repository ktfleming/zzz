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

helpText :: Screen -> T.Text
helpText s = case s of
  ProjectAddScreen{}  -> "Enter: Finish adding | ESC: Return without adding"
  ProjectListScreen{} -> "Enter: View project | a: Add Project"
  ProjectDetailsScreen{}
    -> "Enter: View request definition | Left: back | e: Edit Project | a: Add request definition | d: Delete"
  RequestDetailsScreen{} -> "Left: back | e: Edit | d: Delete"
  ProjectEditScreen{}    -> "Enter: Save | ESC: Return without saving"
  RequestAddScreen{}     -> "Enter: Finsh adding | ESC: Return without adding"
  RequestEditScreen{}    -> "Enter: Save | ESC: Return without saving"
  HelpScreen             -> "todo"

helpPanel :: Screen -> Widget Name
helpPanel s = txt $ helpText s
