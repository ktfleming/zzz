{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal
  ( renderModal,
    dismissModal,
    handleConfirm,
  )
where

import Brick
import Brick.BChan
import Brick.Widgets.Border (border)
import Brick.Widgets.Center
  ( center,
    centerLayer,
  )
import Control.Lens
import Control.Monad.IO.Class
import Data.Text (Text)
import Types.AppState
import Types.Brick.CustomEvent
import Types.Brick.Name (Name)
import Types.Classes.Fields
import Types.Modal
import Types.Models.Project (ProjectContext (..))
import Types.Models.RequestDef (RequestDefContext (..))
import Types.Monads
import UI.Environments.Delete
import UI.Environments.List (refreshIfNecessary, showEnvironmentListScreen)
import UI.Projects.Delete
import UI.Projects.Details (showProjectDetails)
import UI.Projects.List (showProjectListScreen)
import UI.RequestDefs.Delete
import UI.Responses.Delete

renderModalText :: Text -> Widget Name
renderModalText = centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center . txtWrap

renderModal :: AppState a -> Modal -> Widget Name
renderModal s m = renderModalText $ case m of
  DeleteProjectModal c -> deleteProjectWarning s c
  DeleteRequestDefModal c -> deleteRequestDefWarning s c
  DeleteEnvironmentModal c -> deleteEnvironmentWarning s c
  DeleteResponseModal _ _ -> deleteResponseWarning
  ConfirmRequestModal _ -> "Are you sure you want to send this request?"

-- Note: right now modals only support one action (e.g. deleting a resource).
handleConfirm :: MonadIO m => Modal -> BChan CustomEvent -> AnyAppState -> m AnyAppState
handleConfirm m chan outer@(AnyAppState tag s) =
  case m of
    DeleteProjectModal c ->
      pure . wrap . showProjectListScreen . deleteProject c $ s
    DeleteRequestDefModal c@(RequestDefContext pid _) ->
      pure . wrap . showProjectDetails (ProjectContext pid) . deleteRequestDef c $ s
    DeleteEnvironmentModal c ->
      pure . wrap . showEnvironmentListScreen . deleteEnvironment c $ s
    DeleteResponseModal c i ->
      pure . refreshIfNecessary . AnyAppState tag . deleteResponse c i $ s
    ConfirmRequestModal c ->
      sendEvent (SendRequest c) chan >> pure outer

dismissModal :: (HasModal a (Maybe Modal)) => a -> a
dismissModal = modal .~ Nothing
