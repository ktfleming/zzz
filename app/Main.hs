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
import           Types.Constants                ( mainSettingsFile )
import           Types.Screen
import           UI.App                         ( uiApp )
import qualified Data.Map.Strict               as Map

getAppStateFromFile :: ExceptT String IO AppState
getAppStateFromFile = ExceptT $ eitherDecode <$> readFile mainSettingsFile

main :: IO ()
main = do
  runOrError :: Either String AppState <- runExceptT $ do
    fileExists <- ExceptT $ Right <$> doesFileExist mainSettingsFile
    state      <- if fileExists
      then getAppStateFromFile
      else ExceptT $ return $ Right AppState { _activeScreen = HelpScreen
                                             , _projects     = Map.empty
                                             }
    ExceptT $ Right <$> defaultMain uiApp state

  case runOrError of
    Left  e -> putStrLn $ "Encountered error reading saved settings:\n" ++ e
    Right s -> putStrLn $ "Final state:\n" ++ show s
