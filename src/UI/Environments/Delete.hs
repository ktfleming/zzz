{-# LANGUAGE OverloadedStrings #-}

module UI.Environments.Delete where

import Control.Lens
import qualified Data.Text as T
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.Environment

deleteEnvironment :: EnvironmentContext -> AppState a -> AppState a
deleteEnvironment (EnvironmentContext eid) = environments . at eid .~ Nothing

deleteEnvironmentWarning :: AppState a -> EnvironmentContext -> T.Text
deleteEnvironmentWarning s c =
  let e = model s c
   in "Are you sure you want to delete environmnent '" <> e ^. name . coerced <> "'?"
