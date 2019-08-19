{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Title
  ( title
  )
where

import           Control.Lens
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Classes.Displayable      ( display )
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Screen

requestDefBaseTitle :: AppState a -> RequestDefContext -> T.Text
requestDefBaseTitle s c@(RequestDefContext pid _) =
  let p = lookupProject s (ProjectContext pid)
      r = lookupRequestDef s c
  in  p ^. name . coerced <> " > " <> r ^. name . coerced

title :: AppState a -> T.Text
title s = case s ^. screen of
  ProjectAddScreen  _           -> "New Project"
  ProjectListScreen _           -> "All Projects"
  ProjectEditScreen    c _      -> let p = lookupProject s c in p ^. name . coerced <> " (Editing)"
  ProjectDetailsScreen c _      -> let p = lookupProject s c in display p
  RequestDefAddScreen  _ _      -> "New Request Definition"
  RequestDefDetailsScreen c _ _ -> requestDefBaseTitle s c
  RequestDefEditScreen c _      -> requestDefBaseTitle s c <> " (Editing)"
  HelpScreen                    -> "Help"
