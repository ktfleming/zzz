{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.RequestDefinition where

import           Data.Aeson          (FromJSON, ToJSON, object, parseJSON,
                                      toJSON, withObject, (.:), (.=))
import qualified Data.Text           as T
import           Lens.Micro.Platform (makeLenses)
import           Types.Classes.Displayable
import           Types.Models.ID            (ProjectID, RequestDefinitionID)


data RequestDefinition = RequestDefinition { _requestDefinitionName :: T.Text } deriving (Show)

makeLenses ''RequestDefinition

instance ToJSON RequestDefinition where
  toJSON RequestDefinition { _requestDefinitionName } =
    object ["name" .= _requestDefinitionName]

instance FromJSON RequestDefinition where
  parseJSON = withObject "RequestDefinition" $ \o -> do
    name <- o .: "name"
    return $ RequestDefinition
      {  _requestDefinitionName = name }

data RequestDefinitionContext = RequestDefinitionContext ProjectID RequestDefinitionID deriving (Show)
data RequestDefinitionListItem = RequestDefinitionListItem RequestDefinitionContext T.Text

instance Displayable RequestDefinition where
  display = _requestDefinitionName

instance Displayable RequestDefinitionListItem where
  display (RequestDefinitionListItem _ name) = name
