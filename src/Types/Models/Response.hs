{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.Response where

import           Control.Lens                   ( coerced
                                                , (^.)
                                                )
import           Control.Lens.TH                ( makeFields )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , object
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , (.:)
                                                , (.=)
                                                )
import           Data.Sequence                  ( Seq )
import qualified Data.Text                     as T
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           Types.Classes.Fields
import           Types.Methods                  ( Method )
import           Types.Models.Header            ( Header )
import           Types.Models.RequestDef        ( RequestBody(..) )
import           Types.Models.Url               ( Url(..) )

-- TODO: should this be ByteString?
newtype ResponseBody = ResponseBody T.Text deriving (Eq, Show, FromJSON, ToJSON)

data Response = Response {
    responseBody :: ResponseBody
  , responseDateTime :: UTCTime
  , responseMethod :: Method
  , responseUrl :: Url
  , responseRequestBody :: RequestBody
  , responseHeaders :: Seq Header
  , responseElapsedTime :: NominalDiffTime
  } deriving (Show, Eq)

makeFields ''Response

instance ToJSON Response where
  toJSON r = object
    [ "body" .= (r ^. body . coerced :: T.Text)
    , "date_time" .= (r ^. dateTime)
    , "method" .= (r ^. method)
    , "url" .= (r ^. url . coerced :: T.Text)
    , "headers" .= (r ^. headers)
    , "request_body" .= (r ^. requestBody . coerced :: T.Text)
    , "elapsed_time" .= (r ^. elapsedTime)
    ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o ->
    Response
      <$> (o .: "body")
      <*> (o .: "date_time")
      <*> (o .: "method")
      <*> (o .: "url")
      <*> (o .: "request_body")
      <*> (o .: "headers")
      <*> (o .: "elapsed_time")

instance Displayable Response where
  display r = T.pack $ formatISO8601 (r ^. dateTime)
