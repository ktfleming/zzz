{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Models.Id where

import           Data.Aeson
import           Data.UUID                      ( UUID )

newtype ProjectId = ProjectId UUID deriving (Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Eq, Ord)
newtype RequestDefinitionId = RequestDefinitionId UUID deriving (Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Eq, Ord)
