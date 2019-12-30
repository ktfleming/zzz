{-# LANGUAGE OverloadedStrings #-}

module Types.Constants where

import Data.Text (Text)

mainSettingsFile :: String
mainSettingsFile = "zzz.json"

responseHistoryFile :: String
responseHistoryFile = "responses.json"

logFile :: String
logFile = "zzz.log"

-- Dhall requires Text
configFile :: Text
configFile = "./zzz.dhall"
