{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RebindableSyntax #-}

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
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState                 ( AnyAppState(..)
                                                , AppState
                                                , modal
                                                , screen
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
import           Types.Models.Screen            ( Screen(..) )
import           UI.Environments.Delete         ( deleteEnvironment
                                                , deleteEnvironmentWarning
                                                )
import           UI.Environments.List           ( showEnvironmentListScreen )
import           UI.RequestDefs.Details         ( refreshResponseList )
import           UI.Responses.Delete            ( deleteResponse
                                                , deleteResponseWarning
                                                )
import           Utils.IxState                  ( submerge
                                                , (>>>)
                                                )

renderModalText :: T.Text -> Widget Name
renderModalText t =
  (centerLayer . border . hLimitPercent 50 . vLimitPercent 30 . center) $ txtWrap t

renderModal :: AppState a -> Modal -> Widget Name
renderModal s m = case m of
  DeleteProjectModal     c -> renderModalText $ deleteProjectWarning s c
  DeleteRequestDefModal  c -> renderModalText $ deleteRequestDefWarning s c
  DeleteEnvironmentModal c -> renderModalText $ deleteEnvironmentWarning s c
  DeleteResponseModal _ _  -> renderModalText deleteResponseWarning

-- Note: right now modals only support one action (e.g. deleting a resource).
handleConfirm :: Monad m => Modal -> IxStateT m AnyAppState AnyAppState ()
handleConfirm m = iget >>>= \(AnyAppState s) -> case m of
  DeleteProjectModal c -> iput s >>> deleteProject c >>> showProjectListScreen >>> submerge
  DeleteRequestDefModal c@(RequestDefContext pid _) ->
    iput s >>> deleteRequestDef c >>> showProjectDetails (ProjectContext pid) >>> submerge
  DeleteEnvironmentModal c ->
    iput s >>> deleteEnvironment c >>> showEnvironmentListScreen >>> submerge
  DeleteResponseModal c i -> do
    iput s
    deleteResponse c i
    case s ^. screen of
      -- Note: this `RequestDefDetailsScreen` branch should always be the case, but for
      -- now it's necessary since this function runs in AnyAppState while `refreshResponseList`
      -- runs in a tagged AppState.
      RequestDefDetailsScreen{} -> refreshResponseList >>> submerge
      _                         -> submerge

dismissModal :: Monad m => IxStateT m AnyAppState AnyAppState ()
dismissModal = iget >>>= \(AnyAppState s) -> iput (s & modal .~ Nothing) >>> submerge
