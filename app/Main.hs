module Main where

import Brick (defaultMain)
import UI.App
import Types.AppState

main :: IO ()
main = do
  let initialState = AppState { allProjects = [] }
  finalState <- defaultMain uiApp initialState
  return ()
