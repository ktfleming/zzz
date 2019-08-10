{-# LANGUAGE OverloadedStrings #-}

module UI.Console
  ( console
  , toggleConsole
  )
where

import           Brick                          ( Widget
                                                , txt
                                                )
import           Control.Lens
import           Data.Coerce                    ( coerce )
import qualified Data.Text                     as T
import           Types.AppState                 ( AppState
                                                , Message(..)
                                                , screen
                                                , stashedScreen
                                                )
import           Types.Brick.Name               ( Name )
import           Types.Models.Screen

console :: [Message] -> Widget Name
console ms = txt $ T.intercalate "\n" $ coerce ms
--console _ = txt "Console!"

toggleConsole :: AppState -> AppState
toggleConsole s = case s ^. screen of
  ConsoleScreen -> hideConsole s
  _             -> showConsole s

showConsole :: AppState -> AppState
showConsole s =
  let currentScreen = s ^. screen
  in  s & stashedScreen ?~ currentScreen & screen .~ ConsoleScreen

hideConsole :: AppState -> AppState
hideConsole s =
  let stashed = s ^. stashedScreen
  in  case stashed of
        Just sc -> s & screen .~ sc & stashedScreen .~ Nothing
        Nothing -> s -- TODO: what to do here?
