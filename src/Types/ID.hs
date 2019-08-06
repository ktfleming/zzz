{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types.ID where

import           Data.UUID                      ( UUID )
import           Data.Aeson

newtype ProjectID = ProjectID UUID deriving (Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Eq, Ord)
newtype RequestDefinitionID = RequestDefinitionID UUID deriving (Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Eq, Ord)
