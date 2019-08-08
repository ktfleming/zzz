module UI.Modal (renderModal, handleConfirm, dismissModal) where

import Types.Models.Project (ProjectContext(..))
import Brick (hLimitPercent, vLimitPercent, txtWrap, Widget)
import Types.Brick.Name (Name)
import Brick.Widgets.Center (centerLayer, center)
import Brick.Widgets.Border (border)
import qualified Data.Text as T
import Types.Classes.Deletable (delete, warning)
import Types.Modal
import Types.AppState (AppState, modal, activeScreen, _projects)
import Lens.Micro.Platform ((.~), (&))
import Types.Models.Screen (Screen(..))
import Data.Map.Strict (toList)
import UI.Projects.List (makeProjectList)
import Types.Models.RequestDefinition (RequestDefinitionContext(..))
import Types.Classes.ShowDetails (showDetails)

renderModalText :: T.Text -> Widget Name
renderModalText t = (centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center ) $ txtWrap t

renderModal :: AppState -> Modal -> Widget Name
renderModal s m = case m of
  DeleteProjectModal c -> renderModalText $ warning s c
  DeleteRequestDefinitionModal c -> renderModalText $ warning s c

-- Note: right now modals only support one action (e.g. deleting a resource).
handleConfirm :: AppState -> Modal -> AppState
handleConfirm s m = case m of
  DeleteProjectModal c ->
    let stateAfterDeleting = delete s c
        newScreen = ProjectListScreen $ makeProjectList $ toList $ _projects stateAfterDeleting
    in stateAfterDeleting & activeScreen .~ newScreen
  DeleteRequestDefinitionModal c@(RequestDefinitionContext pid _) ->
    let stateAfterDeleting = delete s c
    in showDetails stateAfterDeleting (ProjectContext pid)

dismissModal :: AppState -> AppState
dismissModal = modal .~ Nothing