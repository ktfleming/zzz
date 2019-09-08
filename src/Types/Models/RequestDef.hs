{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.RequestDef where

import           Brick                          ( txt )
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
import           Data.Sequence                  ( Seq )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime )
import           Types.Classes.Displayable
import           Types.Classes.Fields
import           Types.Methods                  ( Method )
import           Types.Models.Header
import           Types.Models.Id                ( ProjectId
                                                , RequestDefId
                                                )
import           Types.Models.Url               ( Url(..) )

newtype RequestDefName = RequestDefName T.Text deriving (FromJSON, ToJSON, Show)
newtype RequestBody = RequestBody T.Text deriving (FromJSON, ToJSON, Show, Eq)
newtype LastError = LastError UTCTime deriving (Show)

data RequestDef = RequestDef {
    requestDefName :: RequestDefName
  , requestDefUrl :: Url
  , requestDefMethod :: Method
  , requestDefBody :: RequestBody
  , requestDefHeaders :: Seq Header
  , requestDefLastError :: Maybe LastError
  } deriving (Show)

data RequestDefFormState = RequestDefFormState {
    requestDefFormStateName :: RequestDefName
  , requestDefFormStateUrl :: Url
  , requestDefFormStateMethod :: Method
  , requestDefFormStateBody :: RequestBody
  , requestDefFormStateHeaders :: Seq Header
  } deriving (Show)

data RequestDefContext = RequestDefContext ProjectId RequestDefId deriving (Show)

data RequestDefListItem = RequestDefListItem RequestDefContext RequestDefName

makeFields ''RequestDef
makeFields ''RequestDefFormState

instance Displayable RequestDefListItem where
  display (RequestDefListItem _ n) = txt $ coerce n

instance ToJSON RequestDef where
  toJSON r = object
    [ "name" .= (r ^. name . coerced :: T.Text)
    , "url" .= (r ^. url . coerced :: T.Text)
    , "method" .= (r ^. method)
    , "body" .= (r ^. body)
    , "headers" .= (r ^. headers)
    ]

instance FromJSON RequestDef where
  parseJSON = withObject "RequestDef" $ \o ->
    RequestDef
      <$> (o .: "name")
      <*> (o .: "url")
      <*> (o .: "method")
      <*> (o .: "body")
      <*> (o .: "headers")
      <*> pure Nothing  -- LastError is not persisted to disk
