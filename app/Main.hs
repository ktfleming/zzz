{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Brick                          ( defaultMain )
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Except
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import qualified Data.HashMap.Strict           as Map
import           Prelude                 hiding ( readFile )
import           System.Directory               ( doesFileExist )
import           Types.AppState
import           Types.Constants                ( mainSettingsFile
                                                , responseHistoryFile
                                                )
import           Types.Models.Screen
import           UI.App                         ( uiApp )
import           UI.Projects.List               ( makeProjectList )

getAppStateFromFile :: ExceptT String IO (AppState 'HelpTag)
getAppStateFromFile = ExceptT $ eitherDecode <$> readFile mainSettingsFile

getResponsesFromFile :: ExceptT String IO Responses
getResponsesFromFile = ExceptT $ eitherDecode <$> readFile responseHistoryFile

main :: IO ()
main = do
  runOrError :: Either String AnyAppState <- runExceptT $ do
    mainFileExists     <- liftIO $ doesFileExist mainSettingsFile
    responseFileExists <- liftIO $ doesFileExist responseHistoryFile
    s                  <- if mainFileExists
      then getAppStateFromFile
      else liftIO $ return emptyAppState
    rs <- if responseFileExists
      then getResponsesFromFile
      else liftIO $ return Map.empty
    -- The default AppState returned by the JSON deserializer starts at the HelpScreen
    -- (done that way to avoid cyclic dependencies), so update the active screen to
    -- ProjectListScreen here, and insert the Responses read from a separate file
    let updatedState =
          s
            &  screen
            .~ ProjectListScreen (makeProjectList (s ^. projects))
            &  responses
            .~ rs
    liftIO $ defaultMain uiApp (AnyAppState updatedState)

  case runOrError of
    Left  e -> putStrLn $ "Encountered error reading saved settings:\n" ++ e
    Right _ -> putStrLn "Done!"
