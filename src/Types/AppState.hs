{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.AppState where

import Control.Concurrent.Async (Async)
import Control.Lens
  ( (&),
    (.~),
    Lens,
    (^.),
    coerced,
    lens,
    view,
  )
import Control.Lens.TH
import Data.Aeson
  ( (.:),
    (.:?),
    (.=),
    FromJSON,
    FromJSONKey,
    ToJSON,
    ToJSONKey,
    object,
    parseJSON,
    toJSON,
    withObject,
  )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Singletons.Decide
  ( (%~),
    Decision (..),
  )
import Data.Time (UTCTime)
import Data.Type.Equality
import GHC.Generics
import Types.Classes.Fields
import Types.Modal
import Types.Models.Environment
import Types.Models.Id
  ( EnvironmentId (..),
    ProjectId,
    RequestDefId,
  )
import Types.Models.Project
import Types.Models.RequestDef
import Types.Models.Response (Response)
import Types.Models.Screen

newtype HelpPanelVisible = HelpPanelVisible Bool deriving (Show, Eq)

data EnvironmentKey = IdKey EnvironmentId | NoEnvironmentKey deriving (Eq, Show, Generic)

-- Just a regular type alias instead of a newtype because I had some trouble getting
-- the lens for this field in `AppState` to work. Could revisit in the future.
type Responses = HashMap RequestDefId (HashMap EnvironmentKey (Seq Response))

newtype AppAsync = AppAsync (Async ()) deriving (Eq)

instance Show AppAsync where
  show _ = "(Async)"

data AppState (a :: ScreenTag)
  = AppState
      { appStateScreen :: Screen a,
        _appStateProjects :: HashMap ProjectId Project,
        _appStateEnvironments :: HashMap EnvironmentId Environment,
        _appStateEnvironmentContext :: Maybe EnvironmentContext, -- the currently selected environment
        _appStateModal :: Maybe Modal,
        _appStateResponses :: Responses,
        _appStateHelpPanelVisible :: HelpPanelVisible,
        _appStateActiveRequests :: HashMap RequestDefId AppAsync,
        _appStateStashedScreen :: Maybe AnyScreen,
        _appStateCurrentTime :: Maybe UTCTime
      }
  deriving (Eq, Show)

makeFields ''AppState

-- Writing this lens by hand instead of using the one from makeFields since it
-- doesn't seem to generate the same thing; does makeFields only generate Lens'?
screen :: Lens (AppState a) (AppState b) (Screen a) (Screen b)
screen = lens getter setter
  where
    getter AppState {appStateScreen} = appStateScreen
    setter appState newScreen = appState {appStateScreen = newScreen}

emptyAppState :: AppState 'HelpTag
emptyAppState = AppState
  { appStateScreen = HelpScreen,
    _appStateProjects = Map.empty,
    _appStateEnvironments = Map.empty,
    _appStateEnvironmentContext = Nothing,
    _appStateModal = Nothing,
    _appStateResponses = Map.empty,
    _appStateHelpPanelVisible = HelpPanelVisible False,
    _appStateActiveRequests = Map.empty,
    _appStateStashedScreen = Nothing,
    _appStateCurrentTime = Nothing
  }

data AnyAppState where
  AnyAppState :: Sing a -> AppState a -> AnyAppState

instance Show AnyAppState where
  show (AnyAppState _ s) = show s

instance Eq AnyAppState where
  AnyAppState t1 s1 == AnyAppState t2 s2 = case t1 %~ t2 of
    Proved Refl -> s1 == s2
    Disproved _ -> False

-- Making instances for HasX for AnyAppState, where X is a field of AppState, will
-- allow us to use those lenses without having to unwrap and wrap it every time.
-- Unfortunately this can't be simplified with a Lens' AnyAppState (AppState a)
-- due to the existential appearing in positive position (at least I think that's why)
-- so we have to jump over the `AppState a` for each individual lens.

instance HasCurrentTime AnyAppState (Maybe UTCTime) where
  currentTime = lens getter setter
    where
      getter (AnyAppState _ s) = s ^. currentTime
      setter (AnyAppState tag s) t = AnyAppState tag (s & currentTime .~ t)

instance HasModal AnyAppState (Maybe Modal) where
  modal = lens getter setter
    where
      getter (AnyAppState _ s) = s ^. modal
      setter (AnyAppState tag s) m = AnyAppState tag (s & modal .~ m)

instance HasProjects AnyAppState (HashMap ProjectId Project) where
  projects = lens getter setter
    where
      getter (AnyAppState _ s) = s ^. projects
      setter (AnyAppState tag s) ps = AnyAppState tag (s & projects .~ ps)

instance HasEnvironments AnyAppState (HashMap EnvironmentId Environment) where
  environments = lens getter setter
    where
      getter (AnyAppState _ s) = s ^. environments
      setter (AnyAppState tag s) es = AnyAppState tag (s & environments .~ es)

instance HasStashedScreen AnyAppState (Maybe AnyScreen) where
  stashedScreen = lens getter setter
    where
      getter (AnyAppState _ s) = s ^. stashedScreen
      setter (AnyAppState tag s) stashed = AnyAppState tag (s & stashedScreen .~ stashed)

instance ToJSON (AppState a) where
  toJSON s =
    object
      [ "projects" .= (s ^. projects),
        "environments" .= (s ^. environments),
        "environment_context" .= (s ^. environmentContext)
      ]

instance FromJSON (AppState 'HelpTag) where
  parseJSON = withObject "AppState" $ \o -> do
    ps <- o .: "projects"
    es <- o .: "environments"
    eid <- o .:? "environment_context"
    pure $ emptyAppState & projects .~ ps & environments .~ es & environmentContext .~ eid

lookupProject :: AppState a -> ProjectContext -> Project
lookupProject s (ProjectContext pid) = (Map.!) (s ^. projects) pid

lookupRequestDef :: AppState a -> RequestDefContext -> RequestDef
lookupRequestDef s (RequestDefContext pid rid) =
  let p = lookupProject s (ProjectContext pid) in (Map.!) (p ^. requestDefs) rid

lookupResponses :: AppState a -> RequestDefContext -> EnvironmentKey -> Seq Response
lookupResponses s (RequestDefContext _ rid) eid = fromMaybe Seq.empty $ do
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
currentVariables s = maybe Seq.empty (view variables) (currentEnvironment s)

instance FromJSON EnvironmentKey

instance FromJSONKey EnvironmentKey

instance ToJSON EnvironmentKey

instance ToJSONKey EnvironmentKey

instance Hashable EnvironmentKey
