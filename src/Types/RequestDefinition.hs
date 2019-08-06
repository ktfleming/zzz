{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.RequestDefinition where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , toJSON
                                                , parseJSON
                                                , object
                                                , (.=)
                                                , withObject
                                                , (.:)
                                                )
import qualified Data.Text                     as T
import           Types.Displayable
import           Types.ID                       ( ProjectID
                                                , RequestDefinitionID
                                                )
import           Lens.Micro.Platform            ( makeLenses )


data RequestDefinition = RequestDefinition { requestDefinitionID :: RequestDefinitionID, _requestDefinitionName :: T.Text } deriving (Show)

makeLenses ''RequestDefinition

instance ToJSON RequestDefinition where
  toJSON RequestDefinition { requestDefinitionID, _requestDefinitionName } =
    object ["id" .= requestDefinitionID, "name" .= _requestDefinitionName]

instance FromJSON RequestDefinition where
  parseJSON = withObject "RequestDefinition" $ \o -> do
    rid  <- o .: "id"
    name <- o .: "name"
    return $ RequestDefinition { requestDefinitionID    = rid
                               , _requestDefinitionName = name
                               }

data RequestDefinitionContext = RequestDefinitionContext ProjectID RequestDefinitionID
data RequestDefinitionListItem = RequestDefinitionListItem RequestDefinitionContext T.Text

instance Displayable RequestDefinition where
  display = _requestDefinitionName

instance Displayable RequestDefinitionListItem where
  display (RequestDefinitionListItem _ name) = name
