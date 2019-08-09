{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Lens.Micro.Platform ((.~), (&))
import UI.Projects.List (makeProjectList)

getAppStateFromFile :: ExceptT String IO AppState
getAppStateFromFile = ExceptT $ eitherDecode <$> readFile mainSettingsFile

main :: IO ()
main = do
  runOrError :: Either String AppState <- runExceptT $ do
    fileExists <- ExceptT $ Right <$> doesFileExist mainSettingsFile
    s@AppState { _projects }      <- if fileExists
      then getAppStateFromFile
      else ExceptT $ return $ Right AppState { _activeScreen = HelpScreen
                                             , _projects     = Map.empty
                                             , _modal        = Nothing
                                             }
    -- The default AppState returned by the JSON deserializer starts at the HelpScreen
    -- (done that way to avoid cyclic dependencies), so update the active screen to
    -- ProjectListScreen here.
    let updatedState = s & activeScreen .~ ProjectListScreen (makeProjectList _projects)
    ExceptT $ Right <$> defaultMain uiApp updatedState

  case runOrError of
    Left  e -> putStrLn $ "Encountered error reading saved settings:\n" ++ e
    Right s -> putStrLn $ "Final state:\n" ++ show s
