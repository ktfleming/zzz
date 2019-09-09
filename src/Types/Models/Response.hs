{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.Response where

import           Brick                          ( Padding(Pad)
                                                , padLeft
                                                , str
                                                , txt
                                                , withAttr
                                                , (<+>)
                                                )
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
import           Numeric                        ( showInt )
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           Types.Classes.Fields
import           Types.Methods                  ( Method )
import           Types.Models.Header            ( Header )
import           Types.Models.RequestDef        ( RequestBody(..) )
import           Types.Models.Url               ( Url(..) )
import           UI.Attr

-- TODO: should this be ByteString?
newtype ResponseBody = ResponseBody T.Text deriving (Eq, Show, FromJSON, ToJSON)
newtype StatusCode = StatusCode Int deriving (Eq, Show, FromJSON, ToJSON)

-- Used for deleting a response by index
newtype ResponseIndex = ResponseIndex Int deriving (Show)

data Response = Response {
    responseBody :: ResponseBody
  , responseStatusCode :: StatusCode
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
    , "status_code" .= (r ^. statusCode . coerced :: Int)
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
      <*> (o .: "status_code")
      <*> (o .: "date_time")
      <*> (o .: "method")
      <*> (o .: "url")
      <*> (o .: "request_body")
      <*> (o .: "headers")
      <*> (o .: "elapsed_time")

-- Displays a status code in color
instance Displayable StatusCode where
  display (StatusCode code) | code >= 200 && code < 300 = withAttr statusCode200Attr w
                            | code >= 300 && code < 400 = withAttr statusCode300Attr w
                            | code >= 400 && code < 500 = withAttr statusCode400Attr w
                            | code >= 500 && code < 600 = withAttr statusCode500Attr w
                            | otherwise                 = w
    where w = str $ showInt code ""

instance Displayable Response where
  display r =
    let sc   = display (r ^. statusCode)
        time = txt $ T.pack $ formatISO8601 (r ^. dateTime)
    in  sc <+> padLeft (Pad 1) time
