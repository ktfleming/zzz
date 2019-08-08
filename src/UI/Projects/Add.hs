{-# LANGUAGE TemplateHaskell #-}

module UI.Projects.Add where

import qualified Data.Text                     as T
import           Lens.Micro.Platform            ( makeLenses )

data ProjectAddState = ProjectAddState { _projectAddName :: T.Text } deriving (Show)

makeLenses ''ProjectAddState
