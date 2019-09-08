{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Methods where

import           Brick                          ( txt
                                                , withAttr
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Sequence                  ( Seq
                                                , fromList
                                                )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Types.Brick.Name
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           UI.Attr                        ( methodAttr )

data Method = Get | Post | Put | Patch deriving (Show, Generic, Eq, Ord)

instance ToJSON Method
instance FromJSON Method

allMethods :: Seq Method
allMethods = fromList [Get, Post, Put, Patch]

allMethodsRadio :: [(Method, Name, T.Text)]
allMethodsRadio =
  [ (Get  , GetRadioField  , "GET")
  , (Post , PostRadioField , "POST")
  , (Put  , PutRadioField  , "PUT")
  , (Patch, PatchRadioField, "PATCH")
  ]

methodToText :: Method -> T.Text
methodToText Get   = "GET"
methodToText Post  = "POST"
methodToText Put   = "PUT"
methodToText Patch = "PATCH"

instance Displayable Method where
  display = withAttr methodAttr . txt . methodToText
