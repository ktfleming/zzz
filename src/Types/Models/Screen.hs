{-# LANGUAGE TemplateHaskell #-}

module Types.Models.Screen where

import           Brick.Focus                    ( FocusRing )
import           Control.Lens                   ( makePrisms )
import           Types.Brick.Name               ( Name )
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Response
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )

-- Represents what main "view" of the app the user is looking at, and also holds the local state for that view
data Screen =
  -- Screen name            Required context      Form/list state                      Others...
  --------------            ----------------      ---------------                      -----------------------
    ProjectAddScreen                              (ZZZForm ProjectFormState)
  | ProjectEditScreen       ProjectContext        (ZZZForm ProjectFormState)
  | ProjectListScreen                             (ZZZList ProjectListItem)
  | ProjectDetailsScreen    ProjectContext        (ZZZList RequestDefListItem)
  | RequestDefDetailsScreen RequestDefContext     (ZZZList Response)                   (FocusRing Name)
  | RequestDefEditScreen    RequestDefContext     (ZZZForm RequestDefFormState)
  | RequestDefAddScreen     ProjectContext        (ZZZForm RequestDefFormState)
  | HelpScreen
  | ConsoleScreen

makePrisms ''Screen

instance Show Screen where
  show _ = "(Screen)"
