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

data Response = Response {
    responseBody :: T.Text
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
