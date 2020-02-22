{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Models.Url where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Text (Text)

newtype Url = Url Text deriving (Show, FromJSON, ToJSON, Eq)
