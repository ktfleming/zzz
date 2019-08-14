{-# LANGUAGE TemplateHaskell #-}

module Types.Models.Screen where

import           Brick.Focus                    ( FocusRing )
import           Control.Lens                   ( makePrisms )
import           Types.Brick.Name               ( Name )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Response
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )

data RequestDetailsSubscreen = ResponseListSubscreen | ResponseBodySubscreen

-- Represents what main "view" of the app the user is looking at, and also holds the local state for that view
data Screen =
  -- Screen name         Required context         Form/list state                      Others...
  --------------         ----------------         ---------------                      -----------------------
    ProjectAddScreen                              (ZZZForm ProjectFormState)
  | ProjectEditScreen    ProjectContext           (ZZZForm ProjectFormState)
  | ProjectListScreen                             (ZZZList ProjectListItem)
  | ProjectDetailsScreen ProjectContext           (ZZZList RequestDefinitionListItem)
  | RequestDetailsScreen RequestDefinitionContext (ZZZList Response)                   (FocusRing Name)
  | RequestEditScreen    RequestDefinitionContext (ZZZForm RequestDefinitionFormState)
  | RequestAddScreen     ProjectContext           (ZZZForm RequestDefinitionFormState)
  | HelpScreen
  | ConsoleScreen

makePrisms ''Screen

instance Show Screen where
  show _ = "(Screen)"
