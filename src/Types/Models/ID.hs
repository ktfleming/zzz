{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types.Models.ID where

import           Data.Aeson
import           Data.UUID  (UUID)

newtype ProjectID = ProjectID UUID deriving (Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Eq, Ord)
newtype RequestDefinitionID = RequestDefinitionID UUID deriving (Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Eq, Ord)
