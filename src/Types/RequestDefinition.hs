{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types.RequestDefinition where

import           Data.Aeson          (FromJSON, ToJSON, object, parseJSON,
                                      toJSON, withObject, (.:), (.=))
import qualified Data.Text           as T
import           Lens.Micro.Platform (makeLenses)
import           Types.Displayable
import           Types.ID            (ProjectID, RequestDefinitionID)


data RequestDefinition = RequestDefinition { requestDefinitionID :: RequestDefinitionID, _requestDefinitionName :: T.Text } deriving (Show)

makeLenses ''RequestDefinition

instance ToJSON RequestDefinition where
  toJSON RequestDefinition { requestDefinitionID, _requestDefinitionName } =
    object ["id" .= requestDefinitionID, "name" .= _requestDefinitionName]

instance FromJSON RequestDefinition where
  parseJSON = withObject "RequestDefinition" $ \o -> do
    rid  <- o .: "id"
    name <- o .: "name"
    return $ RequestDefinition
      { requestDefinitionID    = rid
      , _requestDefinitionName = name
      }

data RequestDefinitionContext = RequestDefinitionContext ProjectID RequestDefinitionID
data RequestDefinitionListItem = RequestDefinitionListItem RequestDefinitionContext T.Text

instance Displayable RequestDefinition where
  display = _requestDefinitionName

instance Displayable RequestDefinitionListItem where
  display (RequestDefinitionListItem _ name) = name
