{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Arrow (left)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile)
import qualified Data.HashMap.Strict as Map
import Data.Time (getCurrentTime)
import Graphics.Vty
  ( defaultConfig,
    mkVty,
  )
import System.Directory (doesFileExist)
import Types.AppState
import Types.Classes.Fields
import Types.Constants
  ( mainSettingsFile,
    responseHistoryFile,
  )
import Types.Models.Screen
import UI.App (uiApp)
import UI.Projects.List (makeProjectList)
import Prelude hiding (readFile)

getAppStateFromFile :: ExceptT String IO (AppState 'HelpTag)
getAppStateFromFile =
  ExceptT $
    left ("Error parsing zzz.json:\n\t" <>) . eitherDecode <$> readFile mainSettingsFile

getResponsesFromFile :: ExceptT String IO Responses
getResponsesFromFile =
  ExceptT $
    left ("Error parsing responses.json:\n\t" <>) . eitherDecode <$> readFile responseHistoryFile

main :: IO ()
main = do
  runOrError :: Either String AnyAppState <- runExceptT $ do
    mainFileExists <- liftIO $ doesFileExist mainSettingsFile
    responseFileExists <- liftIO $ doesFileExist responseHistoryFile
    s <- if mainFileExists then getAppStateFromFile else liftIO $ return emptyAppState
    rs <- if responseFileExists then getResponsesFromFile else liftIO $ return Map.empty
    time <- liftIO getCurrentTime
    -- The default AppState returned by the JSON deserializer starts at the HelpScreen
    -- (done that way to avoid cyclic dependencies), so update the active screen to
    -- ProjectListScreen here, and insert the Responses read from a separate file
    let updatedState =
          s
            & screen
            .~ ProjectListScreen (makeProjectList (s ^. projects))
            & responses
            .~ rs
            & currentTime
            ?~ time
    eventChannel <- liftIO $ newBChan 5
    let buildVty = mkVty defaultConfig
    initialVty <- liftIO buildVty
    liftIO $
      customMain
        initialVty
        buildVty
        (Just eventChannel)
        (uiApp eventChannel)
        (AnyAppState SProjectListTag updatedState)
  case runOrError of
    Left e -> putStrLn e
    Right _ -> putStrLn "Done!"
