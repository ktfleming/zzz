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
import           Control.Monad.Trans.State.Lazy ( StateT
                                                , get
                                                , modify
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Foldable                  ( toList )
import           Data.Sequence                  ( Seq )
import qualified Data.Text                     as T
import           Types.AppState                 ( AppState
                                                , Message(..)
                                                , screen
                                                , stashedScreen
                                                )
import           Types.Brick.Name               ( Name )
import           Types.Models.Screen

console :: Seq Message -> Widget Name
console ms = txt $ T.intercalate "\n" $ coerce (toList ms)

toggleConsole :: Monad m => StateT AppState m ()
toggleConsole = do
  s <- get
  case s ^. screen of
    ConsoleScreen -> hideConsole
    _             -> showConsole

showConsole :: Monad m => StateT AppState m ()
showConsole = do
  s <- get
  let currentScreen = s ^. screen
  modify $ \x -> x & stashedScreen ?~ currentScreen & screen .~ ConsoleScreen

hideConsole :: Monad m => StateT AppState m ()
hideConsole = do
  s <- get
  let stashed = s ^. stashedScreen
  case stashed of
    Just sc -> modify $ \x -> x & screen .~ sc & stashedScreen .~ Nothing
    Nothing -> return () -- TODO: what to do here?
