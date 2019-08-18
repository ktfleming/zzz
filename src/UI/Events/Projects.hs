module UI.Events.Projects where

import           Brick                          ( BrickEvent(VtyEvent) )
import           Brick.Widgets.List             ( listSelectedElement )
import           Control.Lens
import           Graphics.Vty.Input.Events
import           Types.AppState
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectContext
                                                , ProjectFormState
                                                , ProjectListItem(..)
                                                )
import           Types.Models.RequestDef        ( RequestDefListItem(..) )
import           UI.Projects.Add                ( finishAddingProject
                                                , showProjectAddScreen
                                                , updateProjectAddForm
                                                )
import           UI.Projects.Details            ( showProjectDetails
                                                , updateProjectDetailsList
                                                )
import           UI.Projects.Edit               ( finishEditingProject
                                                , showEditProjectScreen
                                                , updateEditProjectForm
                                                )
import           UI.Projects.List               ( showProjectListScreen
                                                , updateProjectList
                                                )
import           UI.RequestDefs.Add             ( showAddRequestDefScreen )
import           UI.RequestDefs.Details         ( showRequestDefDetails )

import           Brick.Types                    ( EventM )
import           Control.Monad.Trans.State      ( modify )
import           Control.Monad.Trans.State.Lazy ( StateT )
import           Types.Brick.Name               ( Name )
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )

handleEventProjectAdd
  :: ZZZForm ProjectFormState -> Key -> StateT AppState (EventM Name) ()
handleEventProjectAdd form key = case key of
  KEnter -> finishAddingProject form
  KEsc   -> showProjectListScreen
  _      -> updateProjectAddForm form (VtyEvent (EvKey key []))

handleEventProjectEdit
  :: ProjectContext
  -> ZZZForm ProjectFormState
  -> Key
  -> StateT AppState (EventM Name) ()
handleEventProjectEdit c form key = case key of
  KEnter -> finishEditingProject c form >> showProjectDetails c
  KEsc   -> showProjectDetails c
  _      -> updateEditProjectForm form (VtyEvent (EvKey key []))

handleEventProjectDetails
  :: ProjectContext
  -> ZZZList RequestDefListItem
  -> Key
  -> StateT AppState (EventM Name) ()
handleEventProjectDetails c list key = case key of
  KEnter -> case listSelectedElement list of
    Just (_, RequestDefListItem reqContext _) ->
      showRequestDefDetails reqContext
    Nothing -> return ()
  KChar 'e' -> showEditProjectScreen c
  KChar 'a' -> showAddRequestDefScreen c
  KChar 'd' -> modify $ modal ?~ DeleteProjectModal c
  KLeft     -> showProjectListScreen
  _         -> updateProjectDetailsList list key

handleEventProjectList
  :: ZZZList ProjectListItem -> Key -> StateT AppState (EventM Name) ()
handleEventProjectList list key = case key of
  KEnter -> case listSelectedElement list of
    Just (_, ProjectListItem context _) -> showProjectDetails context
    Nothing                             -> return ()
  KChar 'a' -> showProjectAddScreen
  _         -> updateProjectList list key
