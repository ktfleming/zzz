{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude                 hiding ( readFile )
import           Brick                          ( defaultMain )
import           UI.App
import           UI.Projects.Add
import           Types.AppState
import           Types.Screen
import           System.Directory               ( doesFileExist )
import           Data.ByteString.Lazy           ( readFile )
import           Data.Aeson                     ( eitherDecode )
import           Control.Monad.Trans.Except

getAppStateFromFile :: ExceptT String IO AppState
getAppStateFromFile = ExceptT $ eitherDecode <$> (readFile "zzz.json")

blankAppState :: AppState
blankAppState = AppState { _activeScreen = ProjectScreen
                         , _allProjects  = []
                         , _activeForm   = noActiveForm
                         }

main :: IO ()
main = do
  runOrError :: Either String AppState <- runExceptT $ do
    fileExists   <- ExceptT $ Right <$> doesFileExist "zzz.json"
    initialState <- if fileExists
      then getAppStateFromFile
      else return blankAppState
    ExceptT $ Right <$> defaultMain uiApp initialState
  case runOrError of
    Left  e -> putStrLn $ "Encountered error reading saved settings:\n" ++ e
    Right s -> putStrLn $ "Final state:\n" ++ (show s)
