{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick (defaultMain)
import UI.App
import UI.Projects.Add
import Types.AppState
import Types.Screen

main :: IO ()
main = do
  let initialState = AppState { _activeScreen = ProjectScreen
                              , _allProjects = []
                              , _activeForm = Just $ mkForm ProjectAddState { _projectName = "New Project" }
                              }
  finalState <- defaultMain uiApp initialState
  return ()
