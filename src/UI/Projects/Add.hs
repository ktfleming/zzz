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
    makeProjectAddForm,
  )
where

import Brick
  ( (<+>),
    txt,
  )
import Brick.Forms
  ( (@@=),
    editTextField,
    formState,
    newForm,
    setFormConcat,
  )
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
import Types.Brick.Name
import Types.Classes.Fields
import Types.Forms (FormMode (..))
import Types.Models.Id (ProjectId (..))
import Types.Models.Project
import Types.Models.Screen
import Types.Monads
  ( IxMonadIO,
    iliftIO,
  )
import UI.Form
  ( AppForm (..),
    spacedConcat,
  )
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

makeProjectAddForm :: AppForm (ProjectFormState 'Adding)
makeProjectAddForm = AppForm $ setFormConcat spacedConcat $ newForm
  [(txt "Project Name: " <+>) @@= editTextField (name . coerced) ProjectFormNameField (Just 1)]
  ProjectFormState {projectFormStateName = ProjectName "New Project"}

showProjectAddScreen :: IxMonadState m => m (AppState a) (AppState 'ProjectAddTag) ()
showProjectAddScreen = imodify $ screen .~ ProjectAddScreen makeProjectAddForm
