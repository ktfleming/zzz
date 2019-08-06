{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Project where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , toJSON
                                                , parseJSON
                                                , object
                                                , (.=)
                                                , (.:)
                                                , withObject
                                                )
import qualified Data.Text                     as T
import           Types.RequestDefinition
import           Types.Displayable              ( Displayable
                                                , display
                                                )
import           Types.ID                       ( ProjectID
                                                , RequestDefinitionID
                                                )
import           Data.Map.Strict                ( Map )
import           Lens.Micro.Platform            ( makeLenses )


data Project = Project { projectID :: ProjectID
                       , _projectName :: T.Text
                       , _requestDefinitions :: Map RequestDefinitionID RequestDefinition} deriving (Show)


makeLenses ''Project

instance ToJSON Project where
  toJSON Project { projectID, _projectName, _requestDefinitions } = object
    [ "id" .= projectID
    , "name" .= _projectName
    , "request_definitions" .= _requestDefinitions
    ]

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> do
    pid     <- o .: "id"
    name    <- o .: "name"
    reqDefs <- o .: "request_definitions"
    return $ Project { projectID           = pid
                     , _projectName        = name
                     , _requestDefinitions = reqDefs
                     }

data ProjectContext = ProjectContext ProjectID
data ProjectListItem = ProjectListItem ProjectContext T.Text

instance Displayable Project where
  display = _projectName

instance Displayable ProjectListItem where
  display (ProjectListItem _ name) = name
