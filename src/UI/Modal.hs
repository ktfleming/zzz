{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Types.AppState                 ( AnyAppState(..)
                                                , AppState
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

import           Control.Monad.Indexed.State    ( IxMonadState
                                                , iget
                                                , imodify
                                                , iput
                                                )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(return, (>>), (>>=))
                                                , pure
                                                )
import           Types.Classes.Fields
import           Types.Models.Screen            ( Screen(..) )
import           Types.Monads
import           UI.Environments.Delete         ( deleteEnvironment
                                                , deleteEnvironmentWarning
                                                )
import           UI.Environments.List           ( showEnvironmentListScreen )
import           UI.RequestDefs.Details         ( refreshResponseList )
import           UI.Responses.Delete            ( deleteResponse
                                                , deleteResponseWarning
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
handleConfirm :: IxMonadState m => Modal -> m AnyAppState AnyAppState ()
handleConfirm m = do
  (AnyAppState s) <- iget
  iput s
  case m of
    DeleteProjectModal c -> sm $ do
      deleteProject c
      showProjectListScreen
    DeleteRequestDefModal c@(RequestDefContext pid _) -> sm $ do
      deleteRequestDef c
      showProjectDetails (ProjectContext pid)
    DeleteEnvironmentModal c -> sm $ do
      deleteEnvironment c
      showEnvironmentListScreen
    DeleteResponseModal c i -> sm $ do
      deleteResponse c i
      case s ^. screen of
        -- Note: this `RequestDefDetailsScreen` branch should always be the case, but for
        -- now it's necessary since this function runs in AnyAppState while `refreshResponseList`
        -- runs in a tagged AppState.
        RequestDefDetailsScreen{} -> refreshResponseList
        _                         -> return ()

dismissModal :: (IxMonadState m, HasModal a (Maybe Modal)) => m a a ()
dismissModal = imodify $ modal .~ Nothing
