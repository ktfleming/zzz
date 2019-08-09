{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Title where

import qualified Data.Text                     as T
import           Types.AppState
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen

title :: AppState -> Screen -> T.Text
title _ (ProjectAddScreen  _) = "New Project"
title _ (ProjectListScreen _) = "All Projects"
title s (ProjectEditScreen c _) =
  let p = lookupProject s c in _projectName p <> " (Editing)"
title s (ProjectDetailsScreen c _) =
  let p = lookupProject s c in _projectName p
title _ (RequestAddScreen _ _) = "New Request Definition"
title s (RequestDetailsScreen c@(RequestDefinitionContext pid _)) =
  let Project { _projectName } = lookupProject s (ProjectContext pid)
      r                        = lookupRequestDefinition s c
  in  _projectName <> " > " <> _requestDefinitionName r
title s (RequestEditScreen c _) =
  title s (RequestDetailsScreen c) <> " (Editing)"
title _ HelpScreen = "Help"
