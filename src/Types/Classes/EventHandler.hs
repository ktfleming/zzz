module Types.Classes.EventHandler where

import           Types.Aliases                  ( EventHandlerFunction )
import           Types.Models.Screen
import           UI.Events.Projects
import           UI.Events.RequestDefs

class EventHandler a where
  handle :: a -> EventHandlerFunction

instance EventHandler Screen where
  handle screen ev = case screen of
    ProjectAddScreen{}        -> handleEventProjectAdd ev
    ProjectDetailsScreen{}    -> handleEventProjectDetails ev
    ProjectEditScreen{}       -> handleEventProjectEdit ev
    ProjectListScreen{}       -> handleEventProjectList ev
    RequestDefDetailsScreen{} -> handleEventRequestDetails ev
    RequestDefAddScreen{}     -> handleEventRequestAdd ev
    RequestDefEditScreen{}    -> handleEventRequestEdit ev
    HelpScreen                -> return ()
    ConsoleScreen             -> return ()
