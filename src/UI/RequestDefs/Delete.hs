{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefs.Delete where

import Control.Lens
import Data.Text (Text)
import Types.AppState
import Types.Classes.Fields
import Types.Models.Project (requestDefs)
import Types.Models.RequestDef

deleteRequestDef :: RequestDefContext -> AppState a -> AppState a
deleteRequestDef (RequestDefContext pid rid) =
  projects . at pid . _Just . requestDefs . at rid .~ Nothing

deleteRequestDefWarning :: AppState a -> RequestDefContext -> Text
deleteRequestDefWarning s c =
  let r = lookupRequestDef s c
   in "Are you sure you want to delete request definition '" <> r ^. name . coerced <> "'?"
