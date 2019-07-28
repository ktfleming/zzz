{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Brick                          ( defaultMain )
import           Control.Monad.Trans.Except
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import           Prelude                 hiding ( readFile )
import           System.Directory               ( doesFileExist )
import           Types.AppState
import           Types.Screen
import           UI.App
import           UI.Projects.Add

getAppStateFromFile :: ExceptT String IO AppState
getAppStateFromFile = ExceptT $ eitherDecode <$> (readFile "zzz.json")

blankAppState :: AppState
blankAppState = AppState { _activeScreen = ProjectListScreen
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
