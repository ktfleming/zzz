{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.AppState where

import           Control.Lens                   ( at
                                                , coerced
                                                , (&)
                                                , (.~)
                                                , (^.)
                                                )
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
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           Types.Modal
import           Types.Models.Id                ( ProjectId
                                                , RequestDefinitionId
                                                )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Response          ( Response )
import           Types.Models.Screen

newtype Message = Message T.Text deriving (Show)
newtype Responses = Responses (HashMap RequestDefinitionId (Seq Response)) deriving (Show, ToJSON, FromJSON)
newtype HelpPanelVisible = HelpPanelVisible Bool deriving (Show)

data AppState = AppState { appStateScreen :: Screen
                         , appStateProjects :: HashMap ProjectId Project
                         , appStateModal :: Maybe Modal
                         , appStateMessages :: Seq Message

                         -- current screen is "stashed" when the user views the console or help
                         -- screen, so it can be restored
                         , appStateStashedScreen :: Maybe Screen

                         , appStateResponses :: Responses
                         , appStateHelpPanelVisible :: HelpPanelVisible
                         } deriving (Show)

makeFields ''AppState

emptyAppState :: AppState
emptyAppState = AppState { appStateScreen           = HelpScreen
                         , appStateProjects         = Map.empty
                         , appStateModal            = Nothing
                         , appStateMessages         = S.empty
                         , appStateStashedScreen    = Nothing
                         , appStateResponses        = Responses Map.empty
                         , appStateHelpPanelVisible = HelpPanelVisible False
                         }

instance ToJSON AppState where
  toJSON s = object ["projects" .= (s ^. projects)]

instance FromJSON AppState where
  parseJSON = withObject "AppState" $ \o -> do
    ps <- o .: "projects"
    return $ emptyAppState & projects .~ ps

lookupProject :: AppState -> ProjectContext -> Project
lookupProject s (ProjectContext pid) = (Map.!) (s ^. projects) pid

lookupRequestDefinition
  :: AppState -> RequestDefinitionContext -> RequestDefinition
lookupRequestDefinition s (RequestDefinitionContext pid rid) =
  let p = lookupProject s (ProjectContext pid)
  in  (Map.!) (p ^. requestDefinitions) rid

lookupResponses :: AppState -> RequestDefinitionContext -> Seq Response
lookupResponses s (RequestDefinitionContext _ rid) =
  let m :: HashMap RequestDefinitionId (Seq Response) =
          s ^. responses . coerced
  in  fromMaybe S.empty (m ^. at rid)
