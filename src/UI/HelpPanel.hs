{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.HelpPanel
  ( helpPanel,
  )
where

import Brick
import qualified Config
import Control.Lens
import Data.Text (Text)
import Types.Brick.Name (Name)
import Types.Classes.Fields
import Types.Models.Screen
import UI.Attr

-- Help messages that appear on every screen; for global keymappings
globalHelp :: Config.AppKeymap -> [(Text, Text)]
globalHelp km =
  [ (Config.describeKey (km ^. searchAll), "Open global search"),
    (Config.describeKey (km ^. showHelp), "Toggle this help panel"),
    (Config.describeKey (km ^. showEnvironments), "Show environment select screen"),
    (Config.describeKey (km ^. quit), "Quit")
  ]

searchHelp :: Config.AppKeymap -> [(Text, Text)]
searchHelp km =
  [ (Config.describeKey (km ^. scrollUp), "Select previous item"),
    (Config.describeKey (km ^. scrollDown), "Select next item")
  ]

-- For now, these are not customizable and use Brick's defaults
formHelp :: Config.AppKeymap -> [(Text, Text)]
formHelp km =
  [ (Config.describeKey (km ^. save), "Add project"),
    (Config.describeKey (km ^. back), "Cancel"),
    ("Tab", "Next field"),
    ("Shift+Tab", "Previous field"),
    ("Left/Up", "Previous radio button"),
    ("Right/Down", "Next radio button")
  ]

helpText :: Config.AppKeymap -> ScreenTag -> [(Text, Text)]
helpText km tag =
  case tag of
    ProjectAddTag -> formHelp km
    ProjectListTag ->
      [ (Config.describeKey (km ^. submit), "View the selected project")
      ]
        <> searchHelp km
    ProjectDetailsTag ->
      [ (Config.describeKey (km ^. submit), "View selected request definition"),
        (Config.describeKey (km ^. back), "Back"),
        (Config.describeKey (km ^. edit), "Edit this project"),
        (Config.describeKey (km ^. add), "Add request definition"),
        (Config.describeKey (km ^. delete), "Delete this project")
      ]
    RequestDefDetailsTag ->
      [ (Config.describeKey (km ^. submit), "Send request"),
        (Config.describeKey (km ^. back), "Back"),
        (Config.describeKey (km ^. edit), "Edit this request definition"),
        (Config.describeKey (km ^. delete), "Delete this request definition"),
        (Config.describeKey (km ^. cancel), "Cancel the active request"),
        (Config.describeKey (km ^. focusNext), "Focus next section (main/history/response"),
        (Config.describeKey (km ^. focusPrev), "Focus previous section (main/history/response)")
      ]
    ProjectEditTag -> formHelp km
    RequestDefAddTag -> formHelp km
    RequestDefEditTag -> formHelp km
    EnvironmentListTag ->
      [ (Config.describeKey (km ^. submit), "Use environment"),
        (Config.describeKey (km ^. add), "Add environment"),
        (Config.describeKey (km ^. edit), "Edit selected environment"),
        (Config.describeKey (km ^. delete), "Delete selected environment")
      ]
        <> searchHelp km
    EnvironmentEditTag -> formHelp km
    EnvironmentAddTag -> formHelp km
    HelpTag -> []
    SearchTag ->
      [ (Config.describeKey (km ^. submit), "Use selected environment / view selected model")
      ]
        <> searchHelp km
    <> globalHelp km

helpPanel :: Config.AppKeymap -> ScreenTag -> Widget Name
helpPanel km tag =
  let oneKeyWidget :: (Text, Text) -> Widget Name
      oneKeyWidget (keyText, actionText) = (withAttr explanationAttr . txt) (keyText <> ": ") <+> txt actionText
   in vBox . fmap oneKeyWidget $ helpText km tag
