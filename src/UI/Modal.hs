{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal
  ( renderModal,
    handleConfirm,
    dismissModal,
  )
where

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border (border)
import Brick.Widgets.Center
  ( center,
    centerLayer,
  )
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.HashSet as HashSet
import Types.AppState
import Types.Brick.CustomEvent
import Types.Brick.Name
import Types.Classes.Fields
import Types.Modal
import Types.Models.Environment
import Types.Models.Project (ProjectContext (..))
import Types.Models.RequestDef (RequestDefContext (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import UI.Environments.Delete
import UI.Environments.List (refreshIfNecessary, showEnvironmentListScreen)
import UI.Form
import UI.Projects.Delete
import UI.Projects.Details (showProjectDetails)
import UI.Projects.List (showProjectListScreen)
import UI.RequestDefs.Delete
import UI.Responses.Delete

centerModal :: Widget Name -> Widget Name
centerModal = centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center

renderModal :: AppState a -> Modal -> Widget Name
renderModal s m = case m of
  DeleteProjectModal c -> centerModal . txt $ deleteProjectWarning s c
  DeleteRequestDefModal c -> centerModal . txt $ deleteRequestDefWarning s c
  DeleteEnvironmentModal c -> centerModal . txt $ deleteEnvironmentWarning s c
  DeleteResponseModal _ _ -> centerModal . txt $ deleteResponseWarning
  ConfirmRequestModal _ -> centerModal . txt $ "Are you sure you want to send this request?"
  VariablePromptModal _ (AppForm form) _ -> centerModal . renderForm $ form

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
    VariablePromptModal c (AppForm form) needsPrompt ->
      case (s ^. screen, formState form) of
        (RequestDefDetailsScreen {}, VariablePromptFormState n v remaining) ->
          -- If no prompt is needed, send the request if no variables are remaining that need to be set.
          -- If a prompt is needed, never send from this branch; instead the ConfirmRequestModal will be
          -- shown after all variables are set, and the request will be sent from that branch.
          let updated = pure . wrap . (screen . rdVariables %~ HashSet.insert (Variable n v)) $ s
              maybeSend =
                if null remaining && not needsPrompt
                  then sendEvent (SendRequest c) chan
                  else pure ()
           in maybeSend >> updated
        _ -> pure outer

-- Update the state when a modal is canceled
dismissModal :: Modal -> AnyAppState -> AnyAppState
dismissModal m (AnyAppState tag s) =
  AnyAppState tag . (modal .~ Nothing) . case (m, s ^. screen) of
    (VariablePromptModal _ _ _, RequestDefDetailsScreen {}) ->
      -- Have to clear the variables that may have already been set in the local
      -- state, if there are any (like if they canceled partway through setting
      -- multiple variables)
      (screen . rdVariables .~ HashSet.empty)
    _ -> id
    $ s
