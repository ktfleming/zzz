{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Brick                          ( defaultMain )
import           Control.Monad.Trans.Except
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import qualified Data.Map.Strict               as Map
import           Prelude                 hiding ( readFile )
import           System.Directory               ( doesFileExist )
import           Types.AppState
import           Types.Constants                ( mainSettingsFile )
import           Types.Models.Screen
import           UI.App                         ( uiApp )

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
                                             , _modal        = Nothing
                                             }
    ExceptT $ Right <$> defaultMain uiApp state

  case runOrError of
    Left  e -> putStrLn $ "Encountered error reading saved settings:\n" ++ e
    Right s -> putStrLn $ "Final state:\n" ++ show s
