{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Models.Url where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Text                     as T

newtype Url = Url T.Text deriving (Show, FromJSON, ToJSON)
