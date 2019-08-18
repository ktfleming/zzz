{-# LANGUAGE DataKinds                  #-}
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

import           Control.Lens                   ( Lens
                                                , at
                                                , coerced
                                                , lens
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
                                                , RequestDefId
                                                )
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Response          ( Response )
import           Types.Models.Screen

newtype Message = Message T.Text deriving (Show)
newtype HelpPanelVisible = HelpPanelVisible Bool deriving (Show)
newtype ConsoleVisible = ConsoleVisible Bool deriving (Show)

-- Just a regular type alias instead of a newtype because I had some trouble getting
-- the lens for this field in `AppState` to work. Could revisit in the future.
type Responses = HashMap RequestDefId (Seq Response)

data AppState (a :: ScreenTag) = AppState {
                            appStateScreen :: Screen a
                         , _appStateProjects :: HashMap ProjectId Project
                         , _appStateModal :: Maybe Modal
                         , _appStateMessages :: Seq Message
                         , _appStateResponses :: HashMap RequestDefId (Seq Response)
                         , _appStateHelpPanelVisible :: HelpPanelVisible
                         , _appStateConsoleVisible :: ConsoleVisible
                         } deriving (Show)

makeFields ''AppState

-- Writing this lens by hand instead of using the one from makeFields since it
-- doesn't seem to generate the same thing; does makeFields only generate Lens'?
screen :: Lens (AppState a) (AppState b) (Screen a) (Screen b)
screen = lens getter setter
 where
  getter AppState { appStateScreen } = appStateScreen
  setter appState newScreen = appState { appStateScreen = newScreen }

emptyAppState :: AppState 'HelpTag
emptyAppState = AppState { appStateScreen            = HelpScreen
                         , _appStateProjects         = Map.empty
                         , _appStateModal            = Nothing
                         , _appStateMessages = S.singleton (Message "Started")
                         , _appStateResponses        = Map.empty
                         , _appStateHelpPanelVisible = HelpPanelVisible False
                         , _appStateConsoleVisible   = ConsoleVisible False
                         }

data AnyAppState where
    AnyAppState ::AppState a -> AnyAppState

instance ToJSON (AppState a) where
  toJSON s = object ["projects" .= (s ^. projects)]

instance FromJSON (AppState 'HelpTag) where
  parseJSON = withObject "AppState" $ \o -> do
    ps <- o .: "projects"
    return $ emptyAppState & projects .~ ps

lookupProject :: AppState a -> ProjectContext -> Project
lookupProject s (ProjectContext pid) = (Map.!) (s ^. projects) pid

lookupRequestDef :: AppState a -> RequestDefContext -> RequestDef
lookupRequestDef s (RequestDefContext pid rid) =
  let p = lookupProject s (ProjectContext pid)
  in  (Map.!) (p ^. requestDefs) rid

lookupResponses :: AppState a -> RequestDefContext -> Seq Response
lookupResponses s (RequestDefContext _ rid) =
  let m :: HashMap RequestDefId (Seq Response) = s ^. responses . coerced
  in  fromMaybe S.empty (m ^. at rid)
