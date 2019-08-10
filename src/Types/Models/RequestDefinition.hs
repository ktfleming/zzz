{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.RequestDefinition where

import           Control.Lens                   ( (^.) )
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
    requestDefinitionName :: T.Text
  , requestDefinitionUrl :: T.Text -- TODO: make this into URL
  , requestDefinitionMethod :: Method
  } deriving (Show)

data RequestDefinitionFormState = RequestDefinitionFormState {
    requestDefinitionFormStateName :: T.Text
  , requestDefinitionFormStateUrl :: T.Text
  , requestDefinitionFormStateMethod :: Method
  } deriving (Show)

data RequestDefinitionContext = RequestDefinitionContext ProjectID RequestDefinitionID deriving (Show)

data RequestDefinitionListItem = RequestDefinitionListItem RequestDefinitionContext T.Text

makeFields ''RequestDefinition
makeFields ''RequestDefinitionFormState

instance Displayable RequestDefinition where
  display r = r ^. name

instance Displayable RequestDefinitionListItem where
  display (RequestDefinitionListItem _ n) = n

instance ToJSON RequestDefinition where
  toJSON r = object
    ["name" .= (r ^. name), "url" .= (r ^. url), "method" .= (r ^. method)]

instance FromJSON RequestDefinition where
  parseJSON = withObject "RequestDefinition" $ \o -> do
    n <- o .: "name"
    u <- o .: "url"
    m <- o .: "method"
    return RequestDefinition { requestDefinitionName   = n
                             , requestDefinitionUrl    = u
                             , requestDefinitionMethod = m
                             }
