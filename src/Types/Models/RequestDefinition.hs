{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.RequestDefinition where

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
import qualified Data.Text                     as T
import           Types.Classes.Displayable
import           Types.Methods                  ( Method )
import           Types.Models.ID                ( ProjectID
                                                , RequestDefinitionID
                                                )

newtype URL = URL T.Text deriving (Show, FromJSON)

data RequestDefinition = RequestDefinition {
    _requestDefinitionName :: T.Text
  , _requestDefinitionURL :: T.Text -- TODO: make this into URL
  , _requestDefinitionMethod :: Method
  } deriving (Show)

data RequestDefinitionFormState = RequestDefinitionFormState {
    _requestDefinitionFormName :: T.Text
  , _requestDefinitionFormURL :: T.Text
  , _requestDefinitionFormMethod :: Method
  } deriving (Show)

data RequestDefinitionContext = RequestDefinitionContext ProjectID RequestDefinitionID deriving (Show)

data RequestDefinitionListItem = RequestDefinitionListItem RequestDefinitionContext T.Text

makeLenses ''RequestDefinition
makeLenses ''RequestDefinitionFormState

instance Displayable RequestDefinition where
  display = _requestDefinitionName

instance Displayable RequestDefinitionListItem where
  display (RequestDefinitionListItem _ name) = name

instance ToJSON RequestDefinition where
  toJSON RequestDefinition { _requestDefinitionName, _requestDefinitionURL, _requestDefinitionMethod }
    = object
      [ "name" .= _requestDefinitionName
      , "url" .= _requestDefinitionURL
      , "method" .= _requestDefinitionMethod
      ]

instance FromJSON RequestDefinition where
  parseJSON = withObject "RequestDefinition" $ \o -> do
    name <- o .: "name"
    u    <- o .: "url"
    m    <- o .: "method"
    return RequestDefinition { _requestDefinitionName   = name
                             , _requestDefinitionURL    = u
                             , _requestDefinitionMethod = m
                             }
