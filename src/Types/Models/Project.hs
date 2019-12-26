{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Models.Project where

import Brick (txt)
import Control.Lens
  ( (^.),
    coerced,
  )
import Control.Lens.TH
import Data.Aeson
  ( (.:),
    (.=),
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
  )
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Types.Classes.Displayable
  ( Displayable,
    display,
  )
import Types.Classes.Fields
import Types.Models.Id
  ( ProjectId,
    RequestDefId,
  )
import Types.Models.RequestDef

newtype ProjectName = ProjectName T.Text deriving (FromJSON, ToJSON, Show, Eq, Ord)

data Project
  = Project
      { projectName :: ProjectName,
        projectRequestDefs :: HashMap RequestDefId RequestDef
      }
  deriving (Show, Eq)

newtype ProjectContext = ProjectContext ProjectId deriving (Show, Eq)

data ProjectListItem = ProjectListItem ProjectContext ProjectName deriving (Show, Eq)

newtype ProjectFormState = ProjectFormState {projectFormStateName :: ProjectName} deriving (Eq, Show)

makeFields ''Project

makeFields ''ProjectFormState

instance ToJSON Project where
  toJSON p =
    object ["name" .= (p ^. name . coerced :: T.Text), "request_definitions" .= (p ^. requestDefs)]

instance FromJSON Project where
  parseJSON =
    withObject "Project" $ \o -> Project <$> (o .: "name") <*> (o .: "request_definitions")

instance Displayable ProjectListItem where
  display (ProjectListItem _ n) = txt $ coerce n
