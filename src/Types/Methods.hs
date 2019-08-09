{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Methods where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Text                     as T
import           Data.Vector                    ( Vector
                                                , fromList
                                                )
import           GHC.Generics                   ( Generic )
import           Types.Brick.Name
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )

data Method = Get | Post | Put | Patch deriving (Show, Generic, Eq, Ord)

instance ToJSON Method
instance FromJSON Method

allMethods :: Vector Method
allMethods = fromList [Get, Post, Put, Patch]

allMethodsRadio :: [(Method, Name, T.Text)]
allMethodsRadio =
  [ (Get  , GetRadioField  , "GET")
  , (Post , PostRadioField , "POST")
  , (Put  , PutRadioField  , "PUT")
  , (Patch, PatchRadioField, "PATCH")
  ]

instance Displayable Method where
  display Get   = "GET"
  display Post  = "POST"
  display Put   = "PUT"
  display Patch = "PATCH"

