{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Add where

import           Brick                          ( str
                                                , (<+>)
                                                )
import           Brick.Forms
import qualified Data.Text                     as T
import           Lens.Micro.Platform            ( makeLenses )
import           Types.CustomEvent
import           Types.Name

data ProjectAddState = ProjectAddState { _projectName :: T.Text } deriving (Show)

makeLenses ''ProjectAddState

mkForm :: ProjectAddState -> Form ProjectAddState CustomEvent Name
mkForm = newForm
  [ (str "Project Name: " <+>)
      @@= editTextField UI.Projects.Add.projectName ProjectAddNameField (Just 1)
  ]
