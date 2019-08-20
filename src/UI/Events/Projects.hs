{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Events.Projects where
import           Brick.Types                    ( EventM )
import           Brick.Widgets.List             ( listSelectedElement )
import           Control.Lens
import           Control.Monad.Indexed          ( ireturn
                                                , (>>>=)
                                                )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Graphics.Vty.Input.Events
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name )
import           Types.Modal                    ( Modal(..) )
import           Types.Models.Project           ( ProjectListItem(..) )
import           Types.Models.RequestDef        ( RequestDefListItem(..) )
import           Types.Models.Screen
import           UI.Events.BrickUpdates         ( updateBrickForm
                                                , updateBrickList
                                                )
import           UI.Projects.Add                ( finishAddingProject
                                                , showProjectAddScreen
                                                )
import           UI.Projects.Details            ( showProjectDetails )
import           UI.Projects.Edit               ( finishEditingProject
                                                , showEditProjectScreen
                                                )
import           UI.Projects.List               ( showProjectListScreen )
import           UI.RequestDefs.Add             ( showAddRequestDefScreen )
import           UI.RequestDefs.Details         ( showRequestDefDetails )
import           Utils.IxState                  ( runOnScreen
                                                , submerge
                                                , (>>>)
                                                )

-- Since each branch of the case expression can lead to a different phantom type
-- parameterizing the state, we can only say that the ultimate output type will be
-- `AnyAppState` (as expected by `handleEventInState`, which calls all of these functions),
-- and we have to apply `submerge` or `runOnScreen` on every branch to ensure the output
-- type is `AnyAppState`.
handleEventProjectAdd :: Key -> IxStateT (EventM Name) (AppState 'ProjectAddTag) AnyAppState ()
handleEventProjectAdd key = case key of
  KEnter -> submerge finishAddingProject
  KEsc   -> submerge showProjectListScreen
  _      -> runOnScreen $ updateBrickForm key

handleEventProjectEdit :: Key -> IxStateT (EventM Name) (AppState 'ProjectEditTag) AnyAppState ()
handleEventProjectEdit key = iget >>>= \s ->
  let ProjectEditScreen c _ = s ^. screen
  in  case key of
        KEnter -> finishEditingProject >>> submerge (showProjectDetails c)
        KEsc   -> submerge $ showProjectDetails c
        _      -> runOnScreen $ updateBrickForm key

handleEventProjectDetails
  :: Key -> IxStateT (EventM Name) (AppState 'ProjectDetailsTag) AnyAppState ()
handleEventProjectDetails key = iget >>>= \s ->
  let ProjectDetailsScreen c list = s ^. screen
  in  case key of
        KEnter -> case listSelectedElement list of
          Just (_, RequestDefListItem reqContext _) -> submerge $ showRequestDefDetails reqContext
          Nothing -> submerge $ ireturn ()
        KChar 'e' -> submerge $ showEditProjectScreen c
        KChar 'a' -> submerge $ showAddRequestDefScreen c
        KChar 'd' -> submerge $ imodify $ modal ?~ DeleteProjectModal c
        KLeft     -> submerge showProjectListScreen
        _         -> runOnScreen $ updateBrickList key

handleEventProjectList :: Key -> IxStateT (EventM Name) (AppState 'ProjectListTag) AnyAppState ()
handleEventProjectList key = iget >>>= \s ->
  let ProjectListScreen list = s ^. screen
  in  case key of
        KEnter -> case listSelectedElement list of
          Just (_, ProjectListItem context _) -> submerge $ showProjectDetails context
          Nothing                             -> submerge $ ireturn ()
        KChar 'a' -> submerge showProjectAddScreen
        _         -> runOnScreen $ updateBrickList key
