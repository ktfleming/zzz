{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Add where

import Brick (Widget, str, (<+>))
import Brick.Forms
import Classes.FormState
import Lens.Micro.Platform
import Types.Name
import Types.CustomEvent
import qualified Data.Text as T

import Debug.Trace

data ProjectAddState = ProjectAddState { _projectName :: T.Text }

instance FormState ProjectAddState where
  submitValid s = trace "submit!" $ return ()

makeLenses ''ProjectAddState

mkForm :: ProjectAddState -> Form ProjectAddState CustomEvent Name
mkForm = newForm [ (str "Project Name: " <+>) @@= editTextField projectName ProjectNameField (Just 1) ]
