{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Types.Models.Screen where

import           Brick.Focus                    ( FocusRing )
import           Brick.Widgets.Edit             ( Editor )
import qualified Data.Text                     as T
import           Types.Brick.Name               ( Name )
import           Types.Models.Environment
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Search                   ( PartitionedResults
                                                , SearchListItem
                                                )
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
  | EnvironmentListTag
  | EnvironmentEditTag
  | EnvironmentAddTag
  | SearchTag
  | HelpTag
  | MessagesTag

-- Represents what main "view" of the app the user is looking at, and also holds the local state for that view
data Screen (a :: ScreenTag) where
  ProjectAddScreen        ::ZZZForm ProjectFormState     ->                                 Screen 'ProjectAddTag
  ProjectEditScreen       ::ProjectContext               -> ZZZForm ProjectFormState ->     Screen 'ProjectEditTag
  ProjectListScreen       ::ZZZList ProjectListItem      ->                                 Screen 'ProjectListTag
  ProjectDetailsScreen    ::ProjectContext               -> ZZZList RequestDefListItem  ->  Screen 'ProjectDetailsTag
  RequestDefEditScreen    ::RequestDefContext            -> ZZZForm RequestDefFormState ->  Screen 'RequestDefEditTag
  RequestDefAddScreen     ::ProjectContext               -> ZZZForm RequestDefFormState ->  Screen 'RequestDefAddTag
  EnvironmentListScreen   ::ZZZList EnvironmentListItem  ->                                 Screen 'EnvironmentListTag
  EnvironmentEditScreen   ::EnvironmentContext           -> ZZZForm EnvironmentFormState -> Screen 'EnvironmentEditTag
  EnvironmentAddScreen    ::ZZZForm EnvironmentFormState ->                                 Screen 'EnvironmentAddTag

  -- This one is getting long enough that it's making the formatting a bit difficult, so let's keep it down here
  RequestDefDetailsScreen ::RequestDefContext -> ZZZList Response -> FocusRing Name -> Maybe RequestError -> Screen 'RequestDefDetailsTag

  SearchScreen            ::Editor T.Text Name -> ZZZList SearchListItem -> PartitionedResults -> Screen 'SearchTag
  HelpScreen              ::Screen 'HelpTag
  MessagesScreen          ::Screen 'MessagesTag

instance Show (Screen a) where
  show _ = "(Screen)"

-- This is used in AppState's `stashedScreen`, using a GADT to hide the phantom type
data AnyScreen where
  AnyScreen ::Screen a -> AnyScreen
