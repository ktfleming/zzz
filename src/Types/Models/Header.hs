{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.Header where

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
import qualified Data.Text                     as T
import           Types.Classes.Fields

newtype HeaderName = HeaderName T.Text deriving (FromJSON, ToJSON, Show, Eq)
newtype HeaderValue = HeaderValue T.Text deriving (FromJSON, ToJSON, Show, Eq)

data Header = Header { headerName :: HeaderName, headerValue :: HeaderValue } deriving (Show, Eq)
makeFields ''Header

-- If a header's name starts with "--", this means that the header should be disabled ("commented
-- out", so to speak).
isHeaderTextEnabled :: T.Text -> Bool
isHeaderTextEnabled t = T.take 2 t /= "--"

isHeaderEnabled :: Header -> Bool
isHeaderEnabled h = isHeaderTextEnabled $ h ^. name . coerced

instance ToJSON Header where
  toJSON h =
    object ["name" .= (h ^. name . coerced :: T.Text), "value" .= (h ^. value . coerced :: T.Text)]

instance FromJSON Header where
  parseJSON = withObject "Header" $ \o -> Header <$> (o .: "name") <*> (o .: "value")
