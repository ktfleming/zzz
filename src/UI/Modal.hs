module UI.Modal
  ( renderModal
  , handleConfirm
  , dismissModal
  )
where

import           Brick                          ( Widget
                                                , hLimitPercent
                                                , txtWrap
                                                , vLimitPercent
                                                )
import           Brick.Widgets.Border           ( border )
import           Brick.Widgets.Center           ( center
                                                , centerLayer
                                                )
import qualified Data.Text                     as T
import           Lens.Micro.Platform            ( (.~) )
import           Types.AppState                 ( AppState
                                                , modal
                                                )
import           Types.Brick.Name               ( Name )
import           Types.Modal
import           Types.Models.Project           ( ProjectContext(..) )
import           Types.Models.RequestDefinition ( RequestDefinitionContext(..) )
import           UI.Projects.Delete             ( deleteProject
                                                , deleteProjectWarning
                                                )
import           UI.Projects.Details            ( showProjectDetails )
import           UI.Projects.List               ( showProjectListScreen )
import           UI.RequestDefinitions.Delete   ( deleteRequestDefinition
                                                , deleteRequestDefinitionWarning
                                                )

renderModalText :: T.Text -> Widget Name
renderModalText t =
  (centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center)
    $ txtWrap t

renderModal :: AppState -> Modal -> Widget Name
renderModal s m = case m of
  DeleteProjectModal c -> renderModalText $ deleteProjectWarning s c
  DeleteRequestDefinitionModal c ->
    renderModalText $ deleteRequestDefinitionWarning s c

-- Note: right now modals only support one action (e.g. deleting a resource).
handleConfirm :: AppState -> Modal -> AppState
handleConfirm s m = case m of
  DeleteProjectModal c -> showProjectListScreen (deleteProject s c)
  DeleteRequestDefinitionModal c@(RequestDefinitionContext pid _) ->
    showProjectDetails (deleteRequestDefinition s c) (ProjectContext pid)

dismissModal :: AppState -> AppState
dismissModal = modal .~ Nothing
