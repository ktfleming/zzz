{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefs.Add where

import Brick.Forms
import Control.Lens
import qualified Data.Sequence as Seq
import Types.AppState
import Types.Classes.Fields
import Types.Methods
import Types.Models.Id (RequestDefId (..))
import Types.Models.Project
  ( ProjectContext (..),
    requestDefs,
  )
import Types.Models.RequestDef
import Types.Models.Screen
import Types.Models.Url (Url (..))
import UI.Form
import UI.RequestDefs.Common (makeRequestDefForm)

finishAddingRequestDef ::
  RequestDefId ->
  AppState 'RequestDefAddTag ->
  AppState 'RequestDefAddTag
finishAddingRequestDef rid s =
  let RequestDefAddScreen (ProjectContext pid) (AppForm form) = s ^. screen
      req =
        RequestDef
          { requestDefName = formState form ^. name,
            requestDefUrl = formState form ^. url,
            requestDefMethod = formState form ^. method,
            requestDefBody = formState form ^. body,
            requestDefHeaders = formState form ^. headers
          }
   in s & projects . at pid . _Just . requestDefs . at rid ?~ req

showAddRequestDefScreen ::
  ProjectContext -> AppState a -> AppState 'RequestDefAddTag
showAddRequestDefScreen c s =
  let fs =
        RequestDefFormState
          { requestDefFormStateName = RequestDefName "",
            requestDefFormStateUrl = Url "",
            requestDefFormStateMethod = Get,
            requestDefFormStateBody = RequestBody "",
            requestDefFormStateHeaders = Seq.empty
          }
      vars = currentVariables s
   in s & screen .~ RequestDefAddScreen c (makeRequestDefForm vars fs)
