{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Types.Models.Response where

import           Control.Lens                   ( (^.) )
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
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )

data Response = Response {
    responseBody :: T.Text -- TODO: should this be ByteString?
  , responseDateTime :: UTCTime
  } deriving (Show, Eq)

makeFields ''Response

instance ToJSON Response where
  toJSON r = object ["body" .= (r ^. body), "date_time" .= (r ^. dateTime)]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    b  <- o .: "body"
    dt <- o .: "date_time"
    return $ Response { responseBody = b, responseDateTime = dt }

instance Displayable Response where
  display r = T.pack $ formatISO8601 (r ^. dateTime)
