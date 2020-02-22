{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens
import Data.Char (toLower)
import Data.Coerce
import qualified Data.Text as T
import Data.Time
import Data.Validation
import Dhall
import Graphics.Vty.Input
import Types.Classes.Fields
import Types.SafetyLevel

newtype ConfigPath = ConfigPath String deriving (Semigroup)

data DhallOffset
  = DhallOffset
      { minutes :: Integer,
        summerOnly :: Bool
      }
  deriving (Generic)

data DhallKey
  = DhallKey
      { key :: String,
        modifiers :: [String]
      }
  deriving (Generic)

data DhallKeymap
  = DhallKeymap
      { quit :: DhallKey,
        save :: DhallKey,
        back :: DhallKey,
        add :: DhallKey,
        edit :: DhallKey,
        delete :: DhallKey,
        cancel :: DhallKey,
        submit :: DhallKey,
        focusNext :: DhallKey,
        focusPrev :: DhallKey,
        scrollUp :: DhallKey,
        scrollDown :: DhallKey,
        showHelp :: DhallKey,
        searchAll :: DhallKey,
        showEnvironments :: DhallKey
      }
  deriving (Generic)

data DhallConfig
  = DhallConfig
      { offset :: DhallOffset,
        keymap :: DhallKeymap,
        safetyLevel :: String
      }
  deriving (Generic)

instance FromDhall DhallOffset

instance FromDhall DhallKey

instance FromDhall DhallKeymap

instance FromDhall DhallConfig

data AppKey = AppKey Key [Modifier]

data AppKeymap
  = AppKeymap
      { _appKeymapQuit :: AppKey,
        _appKeymapSave :: AppKey,
        _appKeymapBack :: AppKey,
        _appKeymapAdd :: AppKey,
        _appKeymapEdit :: AppKey,
        _appKeymapDelete :: AppKey,
        _appKeymapCancel :: AppKey,
        _appKeymapSubmit :: AppKey,
        _appKeymapFocusNext :: AppKey,
        _appKeymapFocusPrev :: AppKey,
        _appKeymapScrollUp :: AppKey,
        _appKeymapScrollDown :: AppKey,
        _appKeymapShowHelp :: AppKey,
        _appKeymapSearchAll :: AppKey,
        _appKeymapShowEnvironments :: AppKey
      }

data AppConfig
  = AppConfig
      { _appConfigTimeZone :: TimeZone,
        _appConfigKeymap :: AppKeymap,
        _appConfigSafetyLevel :: SafetyLevel
      }

makeFields ''AppKeymap

makeFields ''AppConfig

validateKey :: ConfigPath -> String -> Validation [String] Key
validateKey _ [c] = Success (KChar c)
validateKey path word
  | fmap toLower word == "esc" = Success KEsc
  | fmap toLower word == "enter" = Success KEnter
  | fmap toLower word == "tab" = Success (KChar '\t')
  | fmap toLower word == "backtab" = Success KBackTab
  | fmap toLower word == "up" = Success KUp
  | fmap toLower word == "down" = Success KDown
  | fmap toLower word == "left" = Success KLeft
  | fmap toLower word == "right" = Success KRight
  | fmap toLower word == "backspace" = Success KBS
  | fmap toLower word == "printscreen" = Success KPrtScr
  | fmap toLower word == "pause" = Success KPause
  | fmap toLower word == "insert" = Success KIns
  | fmap toLower word == "home" = Success KHome
  | fmap toLower word == "pageup" = Success KPageUp
  | fmap toLower word == "pagedown" = Success KPageDown
  | fmap toLower word == "delete" = Success KDel
  | fmap toLower word == "end" = Success KEnd
  | fmap toLower word == "begin" = Success KBegin
  | fmap toLower word == "menu" = Success KMenu
  | fmap toLower word == "f1" = Success (KFun 1)
  | fmap toLower word == "f2" = Success (KFun 2)
  | fmap toLower word == "f3" = Success (KFun 3)
  | fmap toLower word == "f4" = Success (KFun 4)
  | fmap toLower word == "f5" = Success (KFun 5)
  | fmap toLower word == "f6" = Success (KFun 6)
  | fmap toLower word == "f7" = Success (KFun 7)
  | fmap toLower word == "f8" = Success (KFun 8)
  | fmap toLower word == "f9" = Success (KFun 9)
  | fmap toLower word == "f10" = Success (KFun 10)
  | fmap toLower word == "f11" = Success (KFun 11)
  | fmap toLower word == "f12" = Success (KFun 12)
  | otherwise = Failure [coerce path <> ": unrecognized key '" <> word <> "'"]

validateModifier :: ConfigPath -> String -> Validation [String] Modifier
validateModifier path code = case toLower <$> code of
  "ctrl" -> Success MCtrl
  _ -> Failure [coerce path <> ": unrecognized modifier"]

validateAppKey :: ConfigPath -> DhallKey -> Validation [String] AppKey
validateAppKey path DhallKey {key, modifiers} = AppKey <$> validateKey path key <*> traverse (validateModifier path) modifiers

validateAppKeymap :: DhallKeymap -> Validation [String] AppKeymap
validateAppKeymap dkm =
  -- helper function to cut down on duplication
  let val path getter = validateAppKey (ConfigPath "keymap." <> ConfigPath path) (getter dkm)
   in AppKeymap <$> val "quit" Config.quit
        <*> val "save" Config.save
        <*> val "back" Config.back
        <*> val "add" Config.add
        <*> val "edit" Config.edit
        <*> val "delete" Config.delete
        <*> val "cancel" Config.cancel
        <*> val "submit" Config.submit
        <*> val "focusNext" Config.focusNext
        <*> val "focusPrev" Config.focusPrev
        <*> val "scrollUp" Config.scrollUp
        <*> val "scrollDown" Config.scrollDown
        <*> val "showHelp" Config.showHelp
        <*> val "searchAll" Config.searchAll
        <*> val "showEnvironments" Config.showEnvironments

validateSafetyLevel :: String -> Validation [String] SafetyLevel
validateSafetyLevel "never_prompt" = Success NeverPrompt
validateSafetyLevel "prompt_for_possibly_unsafe" = Success PromptForPossiblyUnsafe
validateSafetyLevel "always_prompt" = Success AlwaysPrompt
validateSafetyLevel other = Failure ["Unrecognized safety level '" <> other <> "'"]

fromDhallConfig :: DhallConfig -> Validation [String] AppConfig
fromDhallConfig dc =
  let offset' = Config.offset dc
      keymap' = Config.keymap dc
      tz = TimeZone (fromIntegral (minutes offset')) (summerOnly offset') ""
      safetyLevel' = Config.safetyLevel dc
   in AppConfig <$> pure tz <*> validateAppKeymap keymap' <*> validateSafetyLevel safetyLevel'

modLabel :: Modifier -> Text
modLabel MShift = "Shift"
modLabel MCtrl = "Ctrl"
modLabel MMeta = "Meta"
modLabel MAlt = "Alt"

keyLabel :: Key -> Text
keyLabel KEsc = "Esc"
keyLabel KEnter = "Enter"
keyLabel (KChar '\t') = "Tab"
keyLabel (KChar c) = T.singleton c
keyLabel KBackTab = "Backtab"
keyLabel KUp = "Up"
keyLabel KDown = "Down"
keyLabel KLeft = "Left"
keyLabel KRight = "Right"
keyLabel KBS = "Backspace"
keyLabel KPrtScr = "Printscreen"
keyLabel KPause = "Pause"
keyLabel KIns = "Insert"
keyLabel KHome = "Home"
keyLabel KPageUp = "PageUp"
keyLabel KPageDown = "PageDown"
keyLabel KDel = "Delete"
keyLabel KEnd = "End"
keyLabel KBegin = "Begin"
keyLabel KMenu = "Menu"
keyLabel (KFun i) = "F" <> T.pack (show i)
keyLabel KUpLeft = "UpLeft"
keyLabel KUpRight = "UpRight"
keyLabel KDownLeft = "DownLeft"
keyLabel KDownRight = "DownRight"
keyLabel KCenter = "Center"

describeKey :: AppKey -> Text
describeKey (AppKey key mods) =
  let labels = (modLabel <$> mods) <> [keyLabel key]
   in T.intercalate "+" labels
