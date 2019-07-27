{-# LANGUAGE DeriveGeneric #-}

module Types.Project where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Project = Project { projectName :: T.Text } deriving (Show, Generic)

instance ToJSON Project
