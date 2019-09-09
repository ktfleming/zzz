{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.AppState where

import           Control.Concurrent.Async       ( Async )
import           Control.Lens                   ( Lens
                                                , coerced
                                                , lens
                                                , view
                                                , (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Control.Lens.TH
import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON
                                                , ToJSONKey
                                                , object
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           GHC.Generics
import           Types.Classes.Fields
import           Types.Modal
import           Types.Models.Environment
import           Types.Models.Id                ( EnvironmentId(..)
                                                , ProjectId
                                                , RequestDefId
                                                )
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Response          ( Response )
import           Types.Models.Screen

newtype Message = Message T.Text deriving (Show)
newtype HelpPanelVisible = HelpPanelVisible Bool deriving (Show)

-- Just a regular type alias instead of a newtype because I had some trouble getting
-- the lens for this field in `AppState` to work. Could revisit in the future.
data EnvironmentKey = IdKey EnvironmentId | NoEnvironmentKey deriving (Eq, Generic)
type Responses = HashMap RequestDefId (HashMap EnvironmentKey (Seq Response))

data AppState (a :: ScreenTag) = AppState {
                            appStateScreen :: Screen a
                         , _appStateProjects :: HashMap ProjectId Project
                         , _appStateEnvironments :: HashMap EnvironmentId Environment
                         , _appStateEnvironmentContext :: Maybe EnvironmentContext -- the currently selected environment
                         , _appStateModal :: Maybe Modal
                         , _appStateMessages :: Seq Message
                         , _appStateResponses :: Responses
                         , _appStateHelpPanelVisible :: HelpPanelVisible
                         , _appStateActiveRequests :: HashMap RequestDefId (Async ())
                         , _appStateStashedScreen :: Maybe AnyScreen
                         }

makeFields ''AppState

-- Writing this lens by hand instead of using the one from makeFields since it
-- doesn't seem to generate the same thing; does makeFields only generate Lens'?
screen :: Lens (AppState a) (AppState b) (Screen a) (Screen b)
screen = lens getter setter
 where
  getter AppState { appStateScreen } = appStateScreen
  setter appState newScreen = appState { appStateScreen = newScreen }

emptyAppState :: AppState 'HelpTag
emptyAppState = AppState { appStateScreen              = HelpScreen
                         , _appStateProjects           = Map.empty
                         , _appStateEnvironments       = Map.empty
                         , _appStateEnvironmentContext = Nothing
                         , _appStateModal              = Nothing
                         , _appStateMessages           = S.singleton (Message "Started")
                         , _appStateResponses          = Map.empty
                         , _appStateHelpPanelVisible   = HelpPanelVisible False
                         , _appStateActiveRequests     = Map.empty
                         , _appStateStashedScreen      = Nothing
                         }

data AnyAppState where
    AnyAppState ::AppState a -> AnyAppState

instance ToJSON (AppState a) where
  toJSON s = object
    [ "projects" .= (s ^. projects)
    , "environments" .= (s ^. environments)
    , "environment_context" .= (s ^. environmentContext)
    ]

instance FromJSON (AppState 'HelpTag) where
  parseJSON = withObject "AppState" $ \o -> do
    ps  <- o .: "projects"
    es  <- o .: "environments"
    eid <- o .:? "environment_context"
    return $ emptyAppState & projects .~ ps & environments .~ es & environmentContext .~ eid

lookupProject :: AppState a -> ProjectContext -> Project
lookupProject s (ProjectContext pid) = (Map.!) (s ^. projects) pid

lookupRequestDef :: AppState a -> RequestDefContext -> RequestDef
lookupRequestDef s (RequestDefContext pid rid) =
  let p = lookupProject s (ProjectContext pid) in (Map.!) (p ^. requestDefs) rid

lookupResponses :: AppState a -> RequestDefContext -> EnvironmentKey -> Seq Response
lookupResponses s (RequestDefContext _ rid) eid = fromMaybe S.empty $ do
  envMap <- Map.lookup rid (s ^. responses)
  Map.lookup eid envMap

lookupEnvironment :: AppState a -> EnvironmentContext -> Environment
lookupEnvironment s (EnvironmentContext eid) = (Map.!) (s ^. environments) eid

currentEnvironment :: AppState a -> Maybe Environment
currentEnvironment s = lookupEnvironment s <$> s ^. environmentContext

-- For looking up responses
currentEnvironmentKey :: AppState a -> EnvironmentKey
currentEnvironmentKey s = maybe NoEnvironmentKey IdKey (s ^. environmentContext . coerced)

currentVariables :: AppState a -> Seq Variable
currentVariables s = maybe S.empty (view variables) (currentEnvironment s)

instance FromJSON EnvironmentKey
instance FromJSONKey EnvironmentKey
instance ToJSON EnvironmentKey
instance ToJSONKey EnvironmentKey
instance Hashable EnvironmentKey
