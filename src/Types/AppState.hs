{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Types.AppState where

import           Control.Lens                   ( (^.) )
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
import           Types.Models.Id                ( ProjectId )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen

data AppState = AppState { appStateScreen :: Screen
                         , appStateProjects :: Map ProjectId Project
                         , appStateModal :: Maybe Modal
                         } deriving (Show)

makeFields ''AppState

instance ToJSON AppState where
  toJSON s = object ["projects" .= (s ^. projects)]

instance FromJSON AppState where
  parseJSON = withObject "AppState" $ \o -> do
    ps <- o .: "projects"
    return $ AppState { appStateScreen   = HelpScreen
                      , appStateProjects = ps
                      , appStateModal    = Nothing
                      }

lookupProject :: AppState -> ProjectContext -> Project
lookupProject s (ProjectContext pid) = (s ^. projects) ! pid

lookupRequestDefinition
  :: AppState -> RequestDefinitionContext -> RequestDefinition
lookupRequestDefinition s (RequestDefinitionContext pid rid) =
  let p = lookupProject s (ProjectContext pid)
  in  (p ^. requestDefinitions) ! rid
