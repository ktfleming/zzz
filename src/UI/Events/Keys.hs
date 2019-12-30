module UI.Events.Keys where

import qualified Config
import Graphics.Vty.Input.Events

matchKey :: Config.AppKey -> Key -> [Modifier] -> Bool
matchKey (Config.AppKey appKey appMods) key mods = appKey == key && appMods == mods
