{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Projects.Add
  ( finishAddingProject,
    showProjectAddScreen,
  )
where

import Brick.Forms
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import qualified Data.HashMap.Strict as Map
import Data.String (fromString)
import Data.UUID.V4 (nextRandom)
import Language.Haskell.DoNotation
import Types.AppState
import Types.Classes.Fields
import Types.Models.Id (ProjectId (..))
import Types.Models.Project
import Types.Models.Screen
import Types.Monads
  ( IxMonadIO,
    iliftIO,
  )
import UI.Form
import UI.Projects.Common (makeProjectForm)
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

finishAddingProject ::
  (IxMonadState m, IxMonadIO m) => m (AppState 'ProjectAddTag) (AppState 'ProjectAddTag) ()
finishAddingProject = do
  s <- iget
  let ProjectAddScreen (AppForm form) = s ^. screen
  pid <- iliftIO $ ProjectId <$> nextRandom
  let project = Project {projectName = formState form ^. name, projectRequestDefs = Map.empty}
  imodify $ projects . at pid ?~ project

showProjectAddScreen :: IxMonadState m => m (AppState a) (AppState 'ProjectAddTag) ()
showProjectAddScreen =
  let fs = ProjectFormState {projectFormStateName = ProjectName "New Project"}
   in imodify $ screen .~ ProjectAddScreen (makeProjectForm fs)
