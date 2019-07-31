{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           GHC.Generics

data Project = Project { _projectName :: T.Text } deriving (Show, Generic)

instance ToJSON Project where
  toJSON Project {..} = object ["name" .= _projectName]

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> do
    name <- o .: "name"
    return $ Project { _projectName = name }
