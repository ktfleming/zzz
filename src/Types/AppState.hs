{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.AppState where

import           Control.Lens.TH
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , object
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , (.:)
                                                , (.=)
                                                )
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )
import           Types.Modal
import           Types.Models.ID                ( ProjectID )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen

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
    return $ AppState { _activeScreen = HelpScreen
                      , _projects     = ps
                      , _modal        = Nothing
                      }

lookupProject :: AppState -> ProjectContext -> Project
lookupProject AppState { _projects } (ProjectContext pid) = _projects ! pid

lookupRequestDefinition
  :: AppState -> RequestDefinitionContext -> RequestDefinition
lookupRequestDefinition s (RequestDefinitionContext pid rid) =
  let Project { _requestDefinitions } = lookupProject s (ProjectContext pid)
  in  _requestDefinitions ! rid
