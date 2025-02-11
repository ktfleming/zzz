{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Models.Header where

import Control.Lens
  ( (^.),
    coerced,
    from,
    iso,
    view,
  )
import Control.Lens.TH
import Data.Aeson
  ( FromJSON,
    ToJSON,
    parseJSON,
    toJSON,
  )
import Data.Text (Text)
import Types.Classes.Fields
import Types.Models.KeyValue

newtype HeaderName = HeaderName Text deriving (FromJSON, ToJSON, Show, Eq)

newtype HeaderValue = HeaderValue Text deriving (FromJSON, ToJSON, Show, Eq)

data Header = Header {headerName :: HeaderName, headerValue :: HeaderValue} deriving (Show, Eq)

makeFields ''Header

instance ToJSON Header where
  toJSON = toJSON . view keyValueIso

instance FromJSON Header where
  parseJSON = fmap (view (from keyValueIso)) . parseJSON

instance KeyValueIso Header where
  keyValueIso =
    iso
      (\h -> KeyValue (h ^. name . coerced) (h ^. value . coerced))
      (\(KeyValue k v) -> Header {headerName = HeaderName k, headerValue = HeaderValue v})
