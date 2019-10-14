{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Environments.Delete where

import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    imodify,
  )
import qualified Data.Text as T
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.Environment

deleteEnvironment :: IxMonadState m => EnvironmentContext -> m (AppState a) (AppState a) ()
deleteEnvironment (EnvironmentContext eid) = imodify $ environments . at eid .~ Nothing

deleteEnvironmentWarning :: AppState a -> EnvironmentContext -> T.Text
deleteEnvironmentWarning s c =
  let e = model s c
   in "Are you sure you want to delete environmnent '" <> e ^. name . coerced <> "'?"
