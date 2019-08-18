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
import           Control.Lens
import           Control.Monad.Trans.State.Lazy ( StateT
                                                , modify
                                                )
import qualified Data.Text                     as T
import           Types.AppState                 ( AppState
                                                , modal
                                                )
import           Types.Brick.Name               ( Name )
import           Types.Modal
import           Types.Models.Project           ( ProjectContext(..) )
import           Types.Models.RequestDef        ( RequestDefContext(..) )
import           UI.Projects.Delete             ( deleteProject
                                                , deleteProjectWarning
                                                )
import           UI.Projects.Details            ( showProjectDetails )
import           UI.Projects.List               ( showProjectListScreen )
import           UI.RequestDefs.Delete          ( deleteRequestDef
                                                , deleteRequestDefWarning
                                                )

renderModalText :: T.Text -> Widget Name
renderModalText t =
  (centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center)
    $ txtWrap t

renderModal :: AppState -> Modal -> Widget Name
renderModal s m = case m of
  DeleteProjectModal    c -> renderModalText $ deleteProjectWarning s c
  DeleteRequestDefModal c -> renderModalText $ deleteRequestDefWarning s c

-- Note: right now modals only support one action (e.g. deleting a resource).
handleConfirm :: Monad m => Modal -> StateT AppState m ()
handleConfirm m = case m of
  DeleteProjectModal c -> deleteProject c >> showProjectListScreen
  DeleteRequestDefModal c@(RequestDefContext pid _) ->
    deleteRequestDef c >> showProjectDetails (ProjectContext pid)

dismissModal :: Monad m => StateT AppState m ()
dismissModal = modify $ modal .~ Nothing
