{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module UI.Modal
  ( renderModal,
    dismissModal,
    handleConfirm,
  )
where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center
  ( center,
    centerLayer,
  )
import Control.Lens
import qualified Data.Text as T
import Types.AppState
  ( AnyAppState (..),
    AppState,
  )
import Types.Brick.Name (Name)
import Types.Classes.Fields
import Types.Modal
import Types.Models.Project (ProjectContext (..))
import Types.Models.RequestDef (RequestDefContext (..))
import Types.Monads
import UI.Environments.Delete
import UI.Environments.List (showEnvironmentListScreen)
import UI.Events.Environments (refreshIfNecessary)
import UI.Projects.Delete
import UI.Projects.Details (showProjectDetails)
import UI.Projects.List (showProjectListScreen)
import UI.RequestDefs.Delete
import UI.Responses.Delete

renderModalText :: T.Text -> Widget Name
renderModalText = centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center . txtWrap

renderModal :: AppState a -> Modal -> Widget Name
renderModal s m = renderModalText $ case m of
  DeleteProjectModal c -> deleteProjectWarning s c
  DeleteRequestDefModal c -> deleteRequestDefWarning s c
  DeleteEnvironmentModal c -> deleteEnvironmentWarning s c
  DeleteResponseModal _ _ -> deleteResponseWarning

-- Note: right now modals only support one action (e.g. deleting a resource).
handleConfirm :: Modal -> AnyAppState -> AnyAppState
handleConfirm m (AnyAppState tag s) =
  case m of
    DeleteProjectModal c ->
      wrap . showProjectListScreen . deleteProject c
    DeleteRequestDefModal c@(RequestDefContext pid _) ->
      wrap . showProjectDetails (ProjectContext pid) . deleteRequestDef c
    DeleteEnvironmentModal c ->
      wrap . showEnvironmentListScreen . deleteEnvironment c
    DeleteResponseModal c i ->
      refreshIfNecessary . AnyAppState tag . deleteResponse c i
    $ s

dismissModal :: (HasModal a (Maybe Modal)) => a -> a
dismissModal = modal .~ Nothing
