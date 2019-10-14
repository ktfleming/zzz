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
import Brick.Forms (renderForm)
import Brick.Types (Padding (Pad))
import Control.Lens
import qualified Data.Text as T
import Types.AppState
import Types.Brick.Name (Name (..))
import Types.Classes.Displayable (Displayable)
import Types.Classes.Fields
import Types.Models.Screen
import UI.Form (AppForm (..))
import UI.List
  ( AppList,
    renderGenericList,
  )
import UI.Messages (messageWidget)
import UI.RequestDefs.Details (requestDefDetailsWidget)
import UI.Search (searchWidget)

formHelpText :: Widget Name
formHelpText =
  txtWrap "Use Tab and Shift-Tab to navigate between fields. Press Enter to save your changes."

-- shortcut for padding a form widget one on top and 2 on the left
padForm :: Widget Name -> Widget Name
padForm = padTop (Pad 1) . padLeft (Pad 2)

listWithExplanation :: Displayable a => AppList a -> T.Text -> Widget Name
listWithExplanation list e = txtWrap e <=> padTop (Pad 1) (renderGenericList True list)

mainWidget :: AnyAppState -> Widget Name
mainWidget (AnyAppState _ s) = case s ^. screen of
  HelpScreen -> txt "Todo"
  ProjectAddScreen (AppForm form) -> renderForm form
  ProjectListScreen list ->
    listWithExplanation list "Select a project to view its details and request definitions."
  ProjectEditScreen _ (AppForm form) -> formHelpText <=> padForm (renderForm form)
  ProjectDetailsScreen _ list ->
    listWithExplanation list "Select a request definition to view its details and send a request."
  RequestDefAddScreen _ (AppForm form) -> renderForm form
  RequestDefDetailsScreen {} -> requestDefDetailsWidget s
  RequestDefEditScreen _ (AppForm form) -> formHelpText <=> padForm (renderForm form)
  EnvironmentListScreen list ->
    listWithExplanation list "Select an environment to view its details."
  EnvironmentEditScreen _ (AppForm form) -> formHelpText <=> padForm (renderForm form)
  EnvironmentAddScreen (AppForm form) -> renderForm form
  SearchScreen edt resultList _ -> searchWidget edt resultList
  MessagesScreen -> messageWidget $ s ^. messages
