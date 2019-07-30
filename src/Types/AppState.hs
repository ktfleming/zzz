{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.AppState where

import           Brick.Forms
import           Brick.Widgets.List             ( GenericList )
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                , FromJSON
                                                , parseJSON
                                                , object
                                                , (.=)
                                                , (.:)
                                                , withObject
                                                )
import           Data.Vector                    ( Vector )
import           Lens.Micro.Platform            ( makeLenses
                                                , (.~)
                                                )
import           Types.CustomEvent
import           Types.Name
import           Types.Project
import           Types.Screen
import           UI.Projects.List


data AppState = AppState { _activeScreen :: Screen
                         , _allProjects :: [Project]
                         } deriving (Show)
makeLenses ''AppState

-- Create the app's initial state; used when either reading the state
-- from a JSON file, or for creating a completely new state when
-- no such file exists.
initialAppState :: [Project] -> AppState
initialAppState ps = AppState
  { _activeScreen = ProjectListScreen $ ListingProjects $ makeProjectList ps
  , _allProjects  = ps
  }

instance ToJSON AppState where
  toJSON AppState {..} = object ["allProjects" .= _allProjects]

instance FromJSON AppState where
  parseJSON = withObject "AppState" $ \o -> do
    projects <- o .: "allProjects"
    return $ initialAppState projects
