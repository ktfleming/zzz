{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module UI.RequestDefs.Edit where

import Brick.Forms
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import Language.Haskell.DoNotation
import Types.AppState
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Forms (FormMode (..))
import Types.Models.Project (requestDefs)
import Types.Models.RequestDef
import Types.Models.Screen
import UI.Form
import UI.RequestDefs.Common
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

finishEditingRequestDef ::
  IxMonadState m => m (AppState 'RequestDefEditTag) (AppState 'RequestDefEditTag) ()
finishEditingRequestDef = do
  s <- iget
  let RequestDefEditScreen c@(RequestDefContext pid rid) (AppForm form) = s ^. screen
      base = model s c
      newModel = updateRequestDef base (formState form)
  imodify $ projects . ix pid . requestDefs . ix rid .~ newModel

updateRequestDef :: RequestDef -> RequestDefFormState 'Editing -> RequestDef
updateRequestDef base form =
  base
    & name
    .~ (form ^. name)
    & url
    .~ (form ^. url)
    & method
    .~ (form ^. method)
    & body
    .~ (form ^. body)
    & headers
    .~ (form ^. headers)

showEditRequestDefScreen ::
  IxMonadState m => RequestDefContext -> m (AppState a) (AppState 'RequestDefEditTag) ()
showEditRequestDefScreen c = do
  s <- iget
  let r = model s c
      vars = currentVariables s
      fs = RequestDefFormState
        { requestDefFormStateName = r ^. name,
          requestDefFormStateUrl = r ^. url,
          requestDefFormStateMethod = r ^. method,
          requestDefFormStateBody = r ^. body,
          requestDefFormStateHeaders = r ^. headers
        }
  imodify $ screen .~ RequestDefEditScreen c (makeRequestDefForm vars fs)
