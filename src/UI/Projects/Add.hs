{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Add where

import Brick (Widget, str, (<+>))
import Brick.Forms
import Lens.Micro.Platform
import Types.Name
import Types.CustomEvent
import qualified Data.Text as T

data ProjectAddState = ProjectAddState { _projectName :: T.Text }

makeLenses ''ProjectAddState

mkForm :: ProjectAddState -> Form ProjectAddState CustomEvent Name
mkForm = newForm [ (str "Project Name: " <+>) @@= editTextField projectName ProjectNameField (Just 1) ]

addProjectWidget :: Widget Name
addProjectWidget = renderForm $ mkForm $ ProjectAddState { _projectName = "New Project" }
