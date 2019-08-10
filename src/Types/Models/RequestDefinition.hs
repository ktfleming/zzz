{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.RequestDefinition where

import           Control.Lens                   ( coerced
                                                , (^.)
                                                )
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
import           Data.Coerce                    ( coerce )
import qualified Data.Text                     as T
import           Types.Classes.Displayable
import           Types.Methods                  ( Method )
import           Types.Models.Id                ( ProjectId
                                                , RequestDefinitionId
                                                )
import           Types.Models.Url               ( Url(..) )

newtype RDName = RDName T.Text deriving (FromJSON, ToJSON, Show)

data RequestDefinition = RequestDefinition {
    requestDefinitionName :: RDName
  , requestDefinitionUrl :: Url
  , requestDefinitionMethod :: Method
  } deriving (Show)

data RequestDefinitionFormState = RequestDefinitionFormState {
    requestDefinitionFormStateName :: RDName
  , requestDefinitionFormStateUrl :: Url
  , requestDefinitionFormStateMethod :: Method
  } deriving (Show)

data RequestDefinitionContext = RequestDefinitionContext ProjectId RequestDefinitionId deriving (Show)

data RequestDefinitionListItem = RequestDefinitionListItem RequestDefinitionContext RDName

makeFields ''RequestDefinition
makeFields ''RequestDefinitionFormState

instance Displayable RequestDefinition where
  display r = r ^. name . coerced

instance Displayable RequestDefinitionListItem where
  display (RequestDefinitionListItem _ n) = coerce n

instance ToJSON RequestDefinition where
  toJSON r = object
    [ "name" .= (r ^. name . coerced :: T.Text)
    , "url" .= (r ^. url . coerced :: T.Text)
    , "method" .= (r ^. method)
    ]

instance FromJSON RequestDefinition where
  parseJSON = withObject "RequestDefinition" $ \o -> do
    n <- o .: "name"
    u <- o .: "url"
    m <- o .: "method"
    return RequestDefinition { requestDefinitionName   = n
                             , requestDefinitionUrl    = u
                             , requestDefinitionMethod = m
                             }
