{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Types.Models.Screen where

import Data.Singletons (Sing)
import Data.Singletons.Decide
  ( (%~),
    Decision (..),
  )
import Data.Singletons.TH
  ( genSingletons,
    singDecideInstances,
  )
import Data.Type.Equality
import Types.Brick.Name (Name)
import Types.Models.Environment
import Types.Models.Project
import Types.Models.RequestDef
import Types.Models.Response
import Types.Search
  ( PartitionedResults,
    SearchListItem,
  )
import UI.Editor (ZZZEditor)
import UI.FocusRing (AppFocusRing)
import UI.Form (AppForm)
import UI.List (AppList)

-- These are only used in conjunction with DataKinds to form phantom type "tags" used on Screen and AppState.
data ScreenTag
  = ProjectAddTag
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
  deriving (Show, Eq)

genSingletons [''ScreenTag]

singDecideInstances [''ScreenTag] -- to be able to use %~ in the Eq instances for AnyScreen, AnyAppState

-- Represents what main "view" of the app the user is looking at, and also holds the local state for that view
data Screen (a :: ScreenTag) where
  ProjectAddScreen ::
    AppForm ProjectFormState ->
    Screen 'ProjectAddTag
  ProjectEditScreen ::
    ProjectContext ->
    AppForm ProjectFormState ->
    Screen 'ProjectEditTag
  ProjectListScreen ::
    AppList ProjectListItem ->
    Screen 'ProjectListTag
  ProjectDetailsScreen ::
    ProjectContext ->
    AppList RequestDefListItem ->
    Screen 'ProjectDetailsTag
  RequestDefEditScreen ::
    RequestDefContext ->
    AppForm RequestDefFormState ->
    Screen 'RequestDefEditTag
  RequestDefAddScreen ::
    ProjectContext ->
    AppForm RequestDefFormState ->
    Screen 'RequestDefAddTag
  RequestDefDetailsScreen ::
    RequestDefContext ->
    AppList Response ->
    AppFocusRing Name ->
    Maybe RequestError ->
    Screen 'RequestDefDetailsTag
  EnvironmentListScreen ::
    AppList EnvironmentListItem ->
    Screen 'EnvironmentListTag
  EnvironmentEditScreen ::
    EnvironmentContext ->
    AppForm EnvironmentFormState ->
    Screen 'EnvironmentEditTag
  EnvironmentAddScreen ::
    AppForm EnvironmentFormState ->
    Screen 'EnvironmentAddTag
  SearchScreen ::
    ZZZEditor ->
    AppList SearchListItem ->
    PartitionedResults ->
    Screen 'SearchTag
  HelpScreen :: Screen 'HelpTag

deriving instance Eq (Screen a)

deriving instance Show (Screen a)

-- This is used in AppState's `stashedScreen`, using a GADT to hide the phantom type
data AnyScreen where
  AnyScreen :: Sing a -> Screen a -> AnyScreen

instance Eq AnyScreen where
  AnyScreen t1 s1 == AnyScreen t2 s2 = case t1 %~ t2 of
    Proved Refl -> s1 == s2
    Disproved _ -> False

instance Show AnyScreen where
  show (AnyScreen _ s) = show s
