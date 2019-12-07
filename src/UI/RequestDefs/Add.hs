{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module UI.RequestDefs.Add where

import Brick.Forms
import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import qualified Data.Sequence as S
import Data.String (fromString)
import Language.Haskell.DoNotation
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
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

finishAddingRequestDef ::
  IxMonadState m =>
  RequestDefId ->
  m (AppState 'RequestDefAddTag) (AppState 'RequestDefAddTag) ()
finishAddingRequestDef rid = do
  s <- iget
  let RequestDefAddScreen (ProjectContext pid) (AppForm form) = s ^. screen
  let req = RequestDef
        { requestDefName = formState form ^. name,
          requestDefUrl = formState form ^. url,
          requestDefMethod = formState form ^. method,
          requestDefBody = formState form ^. body,
          requestDefHeaders = formState form ^. headers
        }
  imodify $ projects . at pid . _Just . requestDefs . at rid ?~ req

showAddRequestDefScreen ::
  IxMonadState m => ProjectContext -> m (AppState a) (AppState 'RequestDefAddTag) ()
showAddRequestDefScreen c = do
  s <- iget
  let fs = RequestDefFormState
        { requestDefFormStateName = RequestDefName "New Request Definition",
          requestDefFormStateUrl = Url "http://example.com",
          requestDefFormStateMethod = Get,
          requestDefFormStateBody = RequestBody "",
          requestDefFormStateHeaders = S.empty
        }
      vars = currentVariables s
  imodify $ screen .~ RequestDefAddScreen c (makeRequestDefForm vars fs)
