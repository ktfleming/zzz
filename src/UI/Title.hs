{-# LANGUAGE OverloadedStrings #-}

module UI.Title where

import           Control.Lens
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Classes.Displayable      ( display )
import           Types.Models.Project
import           Types.Models.RequestDefinition
import           Types.Models.Screen

title :: AppState -> Screen -> T.Text
title _ (ProjectAddScreen  _) = "New Project"
title _ (ProjectListScreen _) = "All Projects"
title s (ProjectEditScreen c _) =
  let p = lookupProject s c in p ^. name . coerced <> " (Editing)"
title s (ProjectDetailsScreen c _) = let p = lookupProject s c in display p
title _ (RequestAddScreen     _ _) = "New Request Definition"
title s (RequestDetailsScreen c@(RequestDefinitionContext pid _)) =
  let p = lookupProject s (ProjectContext pid)
      r = lookupRequestDefinition s c
  in  p ^. name . coerced <> " > " <> r ^. name . coerced
title s (RequestEditScreen c _) =
  title s (RequestDetailsScreen c) <> " (Editing)"
title _ HelpScreen    = "Help"
title _ ConsoleScreen = "Messages"
