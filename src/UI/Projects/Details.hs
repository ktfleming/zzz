{-# LANGUAGE OverloadedStrings #-}

module UI.Projects.Details where

import           Brick                          ( Widget
                                                , txt
                                                )
import qualified Data.Text                     as T
import           Types.Name
import           Types.Project

projectDetailsWidget :: Project -> Widget Name
projectDetailsWidget p = txt $ T.pack "Details for " <> _projectName p
