{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.MainWidget
  ( mainWidget,
  )
where

import Brick
  ( (<=>),
    Widget,
    padLeft,
    padTop,
    txt,
    txtWrap,
  )
import Brick.Types (Padding (Pad))
import Control.Lens
import Types.AppState
import Types.Brick.Name (Name (..))
import Types.Config.Config
import Types.Models.Screen
import UI.Form
import UI.RequestDefs.Details (requestDefDetailsWidget)
import UI.Search.SearchScreen

formHelpText :: Widget Name
formHelpText =
  txtWrap "Use Tab and Shift-Tab to navigate between fields. Press Enter to save your changes."

-- shortcut for padding a form widget one on top and 2 on the left
padForm :: Widget Name -> Widget Name
padForm = padTop (Pad 1) . padLeft (Pad 2)

mainWidget :: Config -> AnyAppState -> Widget Name
mainWidget config (AnyAppState _ s) = case s ^. screen of
  HelpScreen -> txt "Todo"
  ProjectAddScreen (AppForm form) -> renderAppForm form
  ProjectListScreen tools -> searchWidget tools
  ProjectEditScreen _ (AppForm form) -> formHelpText <=> padForm (renderAppForm form)
  ProjectDetailsScreen _ tools -> searchWidget tools
  RequestDefAddScreen _ (AppForm form) -> renderAppForm form
  RequestDefDetailsScreen {} -> requestDefDetailsWidget (config ^. timeZone) s
  RequestDefEditScreen _ (AppForm form) -> formHelpText <=> padForm (renderAppForm form)
  EnvironmentListScreen tools -> searchWidget tools
  EnvironmentEditScreen _ (AppForm form) -> formHelpText <=> padForm (renderAppForm form)
  EnvironmentAddScreen (AppForm form) -> renderAppForm form
  SearchScreen tools -> searchWidget tools
