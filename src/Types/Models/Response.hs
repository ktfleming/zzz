{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Models.Response where

import Brick
  ( (<+>),
    Padding (Pad),
    padLeft,
    str,
    txt,
    withAttr,
  )
import Control.Lens
  ( (^.),
    coerced,
  )
import Control.Lens.TH (makeFields)
import Data.Aeson
  ( (.:),
    (.=),
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
  )
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Formatting
import Formatting.Time
import Numeric (showInt)
import Types.Classes.Displayable
  ( Displayable,
    display,
  )
import Types.Classes.Fields
import Types.Methods (Method)
import Types.Models.Header (Header)
import Types.Models.RequestDef (RequestBody (..))
import Types.Models.Url (Url (..))
import UI.Attr

-- TODO: should this be ByteString?
newtype ResponseBody = ResponseBody Text deriving (Eq, Show, FromJSON, ToJSON)

newtype StatusCode = StatusCode Int deriving (Eq, Show, FromJSON, ToJSON)

-- Used for deleting a response by index
newtype ResponseIndex = ResponseIndex Int deriving (Show, Eq)

data Response
  = Response
      { responseBody :: ResponseBody,
        responseStatusCode :: StatusCode,
        responseDateTime :: UTCTime,
        responseMethod :: Method,
        responseUrl :: Url,
        responseRequestBody :: RequestBody,
        responseHeaders :: Seq Header,
        responseElapsedTime :: NominalDiffTime
      }
  deriving (Show, Eq)

makeFields ''Response

instance ToJSON Response where
  toJSON r =
    object
      [ "body" .= (r ^. body . coerced :: Text),
        "status_code" .= (r ^. statusCode . coerced :: Int),
        "date_time" .= (r ^. dateTime),
        "method" .= (r ^. method),
        "url" .= (r ^. url . coerced :: Text),
        "headers" .= (r ^. headers),
        "request_body" .= (r ^. requestBody . coerced :: Text),
        "elapsed_time" .= (r ^. elapsedTime)
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
  display (StatusCode code)
    | code >= 200 && code < 300 = withAttr statusCode200Attr w
    | code >= 300 && code < 400 = withAttr statusCode300Attr w
    | code >= 400 && code < 500 = withAttr statusCode400Attr w
    | code >= 500 && code < 600 = withAttr statusCode500Attr w
    | otherwise = w
    where
      w = str $ showInt code ""

-- When displaying the response history list, we also need to have the current time
-- in order to calculate the relative time ("X days ago", etc), as well as the TimeZone
-- to use when formatting.
data ResponseHistoryListItem = ResponseHistoryListItem TimeZone UTCTime Response

instance Displayable ResponseHistoryListItem where
  display (ResponseHistoryListItem timeZone utcTime r) =
    let sc = display (r ^. statusCode)
        diffTime = diffUTCTime (r ^. dateTime) utcTime
        relativeTime = sformat (diff True) diffTime -- "a day ago", etc
        zonedTime = utcToZonedTime timeZone (r ^. dateTime)
        absoluteTime = T.pack $ formatTime defaultTimeLocale "%F %T" zonedTime
        timestamp = txt $ absoluteTime <> " (" <> relativeTime <> ")"
     in sc <+> padLeft (Pad 1) timestamp
