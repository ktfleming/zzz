{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Types.Models.Project where

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
import           Data.Map.Strict                ( Map )
import qualified Data.Text                     as T
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           Types.Models.ID                ( ProjectID
                                                , RequestDefinitionID
                                                )
import           Types.Models.RequestDefinition


data Project = Project { projectName :: T.Text
                       , projectRequestDefinitions :: Map RequestDefinitionID RequestDefinition} deriving (Show)

data ProjectContext = ProjectContext ProjectID deriving (Show)
data ProjectListItem = ProjectListItem ProjectContext T.Text

data ProjectFormState = ProjectFormState { projectFormStateName :: T.Text }

makeFields ''Project
makeFields ''ProjectFormState

instance ToJSON Project where
  toJSON p = object
    ["name" .= (p ^. name), "request_definitions" .= (p ^. requestDefinitions)]

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> do
    n       <- o .: "name"
    reqDefs <- o .: "request_definitions"
    return $ Project { projectName = n, projectRequestDefinitions = reqDefs }

instance Displayable Project where
  display p = p ^. name

instance Displayable ProjectListItem where
  display (ProjectListItem _ n) = n
