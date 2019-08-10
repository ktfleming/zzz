{-# LANGUAGE OverloadedStrings #-}

module UI.MainWidget
  ( mainWidget
  )
where

import           Brick                          ( Widget
                                                , padLeft
                                                , padTop
                                                , txt
                                                , txtWrap
                                                , (<=>)
                                                )
import           Brick.Forms                    ( renderForm )
import           Brick.Types                    ( Padding(Pad) )
import           Control.Lens
import           Types.AppState
import           Types.Brick.Name               ( Name )
import           Types.Models.Screen
import           UI.List                        ( renderGenericList )
import           UI.RequestDefinitions.Details  ( requestDefinitionDetailsWidget
                                                )

formHelpText :: Widget Name
formHelpText =
  txtWrap
    "Use Tab and Shift-Tab to navigate between fields. Press Enter to save your changes."

-- shortcut for padding a form widget one on top and 2 on the left
padForm :: Widget Name -> Widget Name
padForm = padTop (Pad 1) . padLeft (Pad 2)

mainWidget :: AppState -> Widget Name
mainWidget s = case s ^. screen of
  HelpScreen            -> txt "Todo"
  ProjectAddScreen form -> renderForm form
  ProjectListScreen list ->
    txtWrap "Select a project to view its details and request definitions."
      <=> padTop (Pad 1) (renderGenericList list)
  ProjectEditScreen _ form -> formHelpText <=> padForm (renderForm form)
  ProjectDetailsScreen _ list ->
    txtWrap
        "Select a request definition to view its details and send a request."
      <=> padTop (Pad 1) (renderGenericList list)
  RequestAddScreen _ form -> renderForm form
  RequestDetailsScreen c ->
    padLeft (Pad 2) $ requestDefinitionDetailsWidget s c
  RequestEditScreen _ form -> formHelpText <=> padForm (renderForm form)
