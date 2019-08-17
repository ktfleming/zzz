module Types.Classes.EventHandler where

import           Brick                          ( continue )
import           Types.Aliases                  ( EventHandlerFunction )
import           Types.Models.Screen
import           UI.Events.Projects
import           UI.Events.RequestDefinitions

class EventHandler a where
  handle :: a -> EventHandlerFunction

instance EventHandler Screen where
  handle screen s ev = case screen of
    ProjectAddScreen{}     -> handleEventProjectAdd s ev
    ProjectDetailsScreen{} -> handleEventProjectDetails s ev
    ProjectEditScreen{}    -> handleEventProjectEdit s ev
    ProjectListScreen{}    -> handleEventProjectList s ev
    RequestDetailsScreen{} -> handleEventRequestDetails s ev
    RequestAddScreen{}     -> handleEventRequestAdd s ev
    RequestEditScreen{}    -> handleEventRequestEdit s ev
    HelpScreen             -> continue s
    ConsoleScreen          -> continue s
