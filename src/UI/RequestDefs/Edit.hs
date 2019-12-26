{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module UI.RequestDefs.Edit where

import Brick.Forms
import Control.Lens
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.Project (requestDefs)
import Types.Models.RequestDef
import Types.Models.Screen
import UI.Form
import UI.RequestDefs.Common

finishEditingRequestDef ::
  AppState 'RequestDefEditTag -> AppState 'RequestDefEditTag
finishEditingRequestDef s =
  let RequestDefEditScreen c@(RequestDefContext pid rid) (AppForm form) = s ^. screen
      base = model s c
      newModel = updateRequestDef (formState form) base
   in s & projects . ix pid . requestDefs . ix rid .~ newModel

updateRequestDef :: RequestDefFormState -> RequestDef -> RequestDef
updateRequestDef form =
  ( name
      .~ (form ^. name)
  )
    . ( url
          .~ (form ^. url)
      )
    . ( method
          .~ (form ^. method)
      )
    . ( body
          .~ (form ^. body)
      )
    . ( headers
          .~ (form ^. headers)
      )

showEditRequestDefScreen ::
  RequestDefContext -> AppState a -> AppState 'RequestDefEditTag
showEditRequestDefScreen c s =
  let r = model s c
      vars = currentVariables s
      fs = RequestDefFormState
        { requestDefFormStateName = r ^. name,
          requestDefFormStateUrl = r ^. url,
          requestDefFormStateMethod = r ^. method,
          requestDefFormStateBody = r ^. body,
          requestDefFormStateHeaders = r ^. headers
        }
   in s & screen .~ RequestDefEditScreen c (makeRequestDefForm vars fs)
