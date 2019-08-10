{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
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
import Control.Lens
import UI.Projects.List (makeProjectList)
import Messages.Messages (logMessage)

getAppStateFromFile :: ExceptT String IO AppState
getAppStateFromFile = ExceptT $ eitherDecode <$> readFile mainSettingsFile

main :: IO ()
main = do
  runOrError :: Either String AppState <- runExceptT $ do
    fileExists <- liftIO $ doesFileExist mainSettingsFile
    s      <- if fileExists
      then getAppStateFromFile
      else liftIO $ return AppState { appStateScreen = HelpScreen
                                             , appStateProjects     = Map.empty
                                             , appStateModal        = Nothing
                                             , appStateMessages = []
                                             , appStateStashedScreen = Nothing
                                             }
    -- The default AppState returned by the JSON deserializer starts at the HelpScreen
    -- (done that way to avoid cyclic dependencies), so update the active screen to
    -- ProjectListScreen here.
    let updatedState = s & screen .~ ProjectListScreen (makeProjectList (s ^. projects))
    logged <- liftIO $ logMessage updatedState "Started"
    liftIO $ defaultMain uiApp logged

  case runOrError of
    Left  e -> putStrLn $ "Encountered error reading saved settings:\n" ++ e
    Right s -> putStrLn $ "Final state:\n" ++ show s
