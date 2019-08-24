{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.Project where

import           Control.Lens                   ( coerced
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
import           Data.Coerce                    ( coerce )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.Text                     as T
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           Types.Classes.Fields
import           Types.Models.Id                ( ProjectId
                                                , RequestDefId
                                                )
import           Types.Models.RequestDef

newtype ProjectName = ProjectName T.Text deriving (FromJSON, ToJSON, Show)

data Project = Project { projectName :: ProjectName
                       , projectRequestDefs :: HashMap RequestDefId RequestDef} deriving (Show)

data ProjectContext = ProjectContext ProjectId deriving (Show)
data ProjectListItem = ProjectListItem ProjectContext ProjectName

data ProjectFormState = ProjectFormState { projectFormStateName :: ProjectName }

makeFields ''Project
makeFields ''ProjectFormState

instance ToJSON Project where
  toJSON p =
    object ["name" .= (p ^. name . coerced :: T.Text), "request_definitions" .= (p ^. requestDefs)]

instance FromJSON Project where
  parseJSON =
    withObject "Project" $ \o -> Project <$> (o .: "name") <*> (o .: "request_definitions")

instance Displayable Project where
  display p = p ^. name . coerced

instance Displayable ProjectListItem where
  display (ProjectListItem _ n) = coerce n
