{-# LANGUAGE TemplateHaskell #-}

module UI.Projects.Edit where

import qualified Data.Text           as T
import           Lens.Micro.Platform (makeLenses)

data ProjectEditState = ProjectEditState { _projectEditName :: T.Text}

makeLenses ''ProjectEditState
