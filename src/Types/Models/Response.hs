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
import           Data.Time                      ( UTCTime )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           Types.Classes.Fields
import           Types.Models.Header            ( Header )
import           Types.Models.Url               ( Url(..) )

-- TODO: should this be ByteString?
newtype ResponseBody = ResponseBody T.Text deriving (Eq, Show, FromJSON, ToJSON)

data Response = Response {
    responseBody :: ResponseBody
  , responseDateTime :: UTCTime
  , responseUrl :: Url
  , responseHeaders :: Seq Header
  } deriving (Show, Eq)

makeFields ''Response

instance ToJSON Response where
  toJSON r = object
    [ "body" .= (r ^. body . coerced :: T.Text)
    , "date_time" .= (r ^. dateTime)
    , "url" .= (r ^. url . coerced :: T.Text)
    , "headers" .= (r ^. headers)
    ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    b  <- o .: "body"
    dt <- o .: "date_time"
    u  <- o .: "url"
    h  <- o .: "headers"
    return
      $ Response { responseBody = b, responseDateTime = dt, responseUrl = u, responseHeaders = h }

instance Displayable Response where
  display r = T.pack $ formatISO8601 (r ^. dateTime)
