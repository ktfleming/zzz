{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import           Lens.Micro.Platform            ( makeLenses )
import           Types.CustomEvent
import           Types.Name
import           Types.Project
import           Types.Screen
import           UI.Projects.List               ( makeProjectList )

class FormState a where
  -- Use the valid model contained in the form to modify the global AppState
  submitValid :: AppState -> a -> AppState

-- The `x` here is the form's state; it's not exposed in the `ActiveForm` type since
-- `ActiveForm` is used generically in `handleEvent` to send events to the form, regardless
-- of which form it is.
data ActiveForm = forall x. FormState x => ActiveForm (Form x CustomEvent Name)
instance Show ActiveForm where
  show _ = "(Form is active)"

-- Similar to `ActiveForm`, but for Brick lists
data ActiveList = forall x. Show x => ActiveList (GenericList Name Vector x)
instance Show ActiveList where
  show _ = "(List is active)"

-- Represents the widget that should receive input events
data EventHandler = FormHandler ActiveForm | ListHandler ActiveList | NoHandler deriving (Show)

data AppState = AppState { _activeScreen :: Screen
                         , _allProjects :: [Project]
                         , _eventHandler :: EventHandler
                         } deriving (Show)
makeLenses ''AppState

handleSubmit
  :: forall x . FormState x => AppState -> Form x CustomEvent Name -> AppState
handleSubmit appState form = submitValid appState (formState form)

-- Create the app's initial state; used when either reading the state
-- from a JSON file, or for creating a completely new state when
-- no such file exists.
initialAppState :: [Project] -> AppState
initialAppState ps = AppState
  { _activeScreen = ProjectListScreen
  , _allProjects  = ps
  , _eventHandler = ListHandler $ ActiveList $ makeProjectList ps
  }

instance ToJSON AppState where
  toJSON AppState {..} = object ["allProjects" .= _allProjects]

instance FromJSON AppState where
  parseJSON = withObject "AppState" $ \o -> do
    projects <- o .: "allProjects"
    return $ initialAppState projects
