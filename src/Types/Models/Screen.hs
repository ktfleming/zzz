{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Types.Models.Screen where

import           Brick.Focus                    ( FocusRing )
import           Types.Brick.Name               ( Name )
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Response
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )

-- These are only used in conjunction with DataKinds to form phantom type "tags" used on Screen and AppState.
-- The phantom type on AppScreen is used to determine valid state inputs and outputs in the event-handling
-- code that runs inside the IxStateT monad stack.
data ScreenTag =
    ProjectAddTag
  | ProjectEditTag
  | ProjectListTag
  | ProjectDetailsTag
  | RequestDefDetailsTag
  | RequestDefEditTag
  | RequestDefAddTag
  | HelpTag

-- Represents what main "view" of the app the user is looking at, and also holds the local state for that view
data Screen (a :: ScreenTag) where

  ProjectAddScreen        ::ZZZForm ProjectFormState ->                                                  Screen 'ProjectAddTag
  ProjectEditScreen       ::ProjectContext           -> ZZZForm ProjectFormState    ->                   Screen 'ProjectEditTag
  ProjectListScreen       ::ZZZList ProjectListItem  ->                                                  Screen 'ProjectListTag
  ProjectDetailsScreen    ::ProjectContext           -> ZZZList RequestDefListItem  ->                   Screen 'ProjectDetailsTag
  RequestDefDetailsScreen ::RequestDefContext        -> ZZZList Response            -> FocusRing Name -> Screen 'RequestDefDetailsTag
  RequestDefEditScreen    ::RequestDefContext        -> ZZZForm RequestDefFormState ->                   Screen 'RequestDefEditTag
  RequestDefAddScreen     ::ProjectContext           -> ZZZForm RequestDefFormState ->                   Screen 'RequestDefAddTag

  HelpScreen              ::Screen 'HelpTag

instance Show (Screen a) where
  show _ = "(Screen)"
