module UI.Modal
  ( renderModal
  , dismissModal
  , handleConfirm
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
import qualified Data.Text                     as T
import           Types.AppState                 ( AnyAppState(..)
                                                , AppState
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

import           Control.Monad.Indexed          ( (>>>=) )
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , iput
                                                )
import           Utils.IxState                  ( submerge
                                                , (>>>)
                                                )

renderModalText :: T.Text -> Widget Name
renderModalText t =
  (centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center) $ txtWrap t

renderModal :: AppState a -> Modal -> Widget Name
renderModal s m = case m of
  DeleteProjectModal    c -> renderModalText $ deleteProjectWarning s c
  DeleteRequestDefModal c -> renderModalText $ deleteRequestDefWarning s c

-- Note: right now modals only support one action (e.g. deleting a resource).
handleConfirm :: Monad m => Modal -> IxStateT m AnyAppState AnyAppState ()
handleConfirm m = iget >>>= \(AnyAppState s) -> case m of
  DeleteProjectModal c -> iput s >>> deleteProject c >>> submerge showProjectListScreen
  DeleteRequestDefModal c@(RequestDefContext pid _) ->
    iput s >>> deleteRequestDef c >>> submerge (showProjectDetails (ProjectContext pid))

dismissModal :: Monad m => IxStateT m AnyAppState AnyAppState ()
dismissModal = iget >>>= \(AnyAppState s) -> submerge $ iput $ s & modal .~ Nothing
