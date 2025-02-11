{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Brick (customMain)
import Brick.BChan (newBChan)
import Config
import Control.Arrow (left)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (readFile)
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate)
import Data.Time (getCurrentTime)
import qualified Data.Validation as Validation
import Dhall
import Graphics.Vty
  ( defaultConfig,
    mkVty,
  )
import System.Directory (doesFileExist)
import Types.AppState
import Types.Classes.Fields
import Types.Constants
import Types.Models.Screen
import UI.App (uiApp)
import UI.Projects.List (showProjectListScreen)
import Prelude hiding (readFile)

getDataFromFile :: FromJSON a => String -> ExceptT String IO a
getDataFromFile file = ExceptT $ left (("Error parsing " <> file <> ":\n\t") <>) . eitherDecode <$> readFile file

formatConfigurationErrors :: [String] -> String
formatConfigurationErrors =
  intercalate "\n" . (["Errors found in configuration file."] <>) . fmap ("  - " <>)

main :: IO ()
main = do
  runOrError :: Either String AnyAppState <- runExceptT $ do
    mainFileExists <- liftIO $ doesFileExist mainSettingsFile
    responseFileExists <- liftIO $ doesFileExist responseHistoryFile
    s <- if mainFileExists then getDataFromFile mainSettingsFile else liftIO $ pure emptyAppState
    rs <- if responseFileExists then getDataFromFile responseHistoryFile else liftIO $ pure Map.empty
    time <- liftIO getCurrentTime
    dhallConfig :: DhallConfig <- liftIO $ input auto configFile
    -- The default AppState returned by the JSON deserializer starts at the HelpScreen
    -- (done that way to avoid cyclic dependencies), so update the active screen to
    -- ProjectListScreen here, and insert the Responses read from a separate file
    let updatedState =
          s
            & responses
            .~ rs
            & currentTime
            .~ time
    eventChannel <- liftIO $ newBChan 5
    let buildVty = mkVty Graphics.Vty.defaultConfig
    initialVty <- liftIO buildVty
    appConfig <- ExceptT . pure . left formatConfigurationErrors . Validation.toEither $ fromDhallConfig dhallConfig
    liftIO $
      customMain
        initialVty
        buildVty
        (Just eventChannel)
        (uiApp appConfig eventChannel)
        (AnyAppState SProjectListTag (showProjectListScreen updatedState))
  case runOrError of
    Left e -> putStrLn e
    Right _ -> putStrLn "Done!"
