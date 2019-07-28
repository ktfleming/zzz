{-# LANGUAGE OverloadedStrings #-}

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

import           Debug.Trace

getAppStateFromFile :: ExceptT String IO AppState
getAppStateFromFile = ExceptT $ eitherDecode <$> (readFile "zzz.json")

blankAppState :: AppState
blankAppState = AppState { _activeScreen = ProjectScreen
                         , _allProjects  = []
                         , _activeForm   = noActiveForm
                         }

main :: IO ()
main = fmap (\_ -> ()) $ runExceptT $ do
  fileExists   <- ExceptT $ Right <$> doesFileExist "zzz.json"
  initialState <- if fileExists
    then getAppStateFromFile
    else return blankAppState
  _ <- ExceptT $ Right <$> putStrLn (show initialState)
  ExceptT $ Right <$> defaultMain uiApp initialState
