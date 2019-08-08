{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Types.AppState where

import           Data.Aeson              (FromJSON, ToJSON, object, parseJSON,
                                          toJSON, withObject, (.:), (.=))
import           Data.Map.Strict         (Map, (!))
import qualified Data.Text               as T
import           Lens.Micro.Platform     (makeLenses)
import           Types.Models.ID                (ProjectID)
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen
import           UI.Projects.List
import Types.Modal

data AppState = AppState { _activeScreen :: Screen
                         , _projects :: Map ProjectID Project
                         , _modal :: Maybe Modal
                         } deriving (Show)

makeLenses ''AppState

instance ToJSON AppState where
  toJSON AppState { _projects } = object ["projects" .= _projects]

instance FromJSON AppState where
  parseJSON = withObject "AppState" $ \o -> do
    ps <- o .: "projects"
    return $ AppState
      { _activeScreen = ProjectListScreen $ makeProjectList ps
      , _projects     = ps
      , _modal        = Nothing
      }

lookupProject :: AppState -> ProjectContext -> Project
lookupProject AppState { _projects } (ProjectContext pid) = _projects ! pid

lookupRequestDefinition
  :: AppState -> RequestDefinitionContext -> RequestDefinition
lookupRequestDefinition s (RequestDefinitionContext pid rid) =
  let Project { _requestDefinitions } = lookupProject s (ProjectContext pid)
  in _requestDefinitions ! rid

title :: AppState -> Screen -> T.Text
title _ (ProjectAddScreen  _  ) = "New Project"
title _ (ProjectListScreen _  ) = "All Projects"
title s (ProjectEditScreen c _) = let p = lookupProject s c in _projectName p
title s (ProjectDetailsScreen c _) =
  let p = lookupProject s c in _projectName p
title _ (RequestAddScreen _ _) = "New Request Definition"
title s (RequestDetailsScreen c) =
  let r = lookupRequestDefinition s c in _requestDefinitionName r
title s (RequestEditScreen c _) =
  let req = lookupRequestDefinition s c in _requestDefinitionName req
title _ HelpScreen = "Help"
