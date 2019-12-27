{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Config.Config where

import Control.Lens.TH
import Data.Time

data Config = Config {_configTimeZone :: TimeZone}

makeFields ''Config

defaultConfig :: Config
defaultConfig =
  Config
    { _configTimeZone = hoursToTimeZone 9
    }
