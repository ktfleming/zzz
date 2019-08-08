{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.Models.Project where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , object
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , (.:)
                                                , (.=)
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Text                     as T
import           Lens.Micro.Platform            ( makeLenses )
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           Types.Models.ID                ( ProjectID
                                                , RequestDefinitionID
                                                )
import           Types.Models.RequestDefinition


data Project = Project { _projectName :: T.Text
                       , _requestDefinitions :: Map RequestDefinitionID RequestDefinition} deriving (Show)


makeLenses ''Project

instance ToJSON Project where
  toJSON Project { _projectName, _requestDefinitions } = object
    ["name" .= _projectName, "request_definitions" .= _requestDefinitions]

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> do
    name    <- o .: "name"
    reqDefs <- o .: "request_definitions"
    return $ Project { _projectName = name, _requestDefinitions = reqDefs }

data ProjectContext = ProjectContext ProjectID deriving (Show)
data ProjectListItem = ProjectListItem ProjectContext T.Text

instance Displayable Project where
  display = _projectName

instance Displayable ProjectListItem where
  display (ProjectListItem _ name) = name
