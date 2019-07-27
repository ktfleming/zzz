module Main where

import Brick (defaultMain)
import UI.App
import Types.AppState
import Types.Screen

main :: IO ()
main = do
  let initialState = AppState { _activeScreen = ProjectScreen
                              , _allProjects = [] }
  finalState <- defaultMain uiApp initialState
  return ()
