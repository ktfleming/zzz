{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module UI.RequestDefs.Add where

import Brick
  ( (<+>),
    txt,
  )
import Brick.Forms
  ( (@@=),
    formState,
    newForm,
    setFormConcat,
  )
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
import Types.Brick.Name
import Types.Classes.Fields
import Types.Forms (FormMode (..))
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
  ( AppForm (..),
    nonEmptyTextField,
    spacedConcat,
  )
import UI.Forms.KeyValueList (makeKeyValueForm)
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

makeRequestDefAddForm :: RequestDefFormState 'Adding -> AppForm (RequestDefFormState 'Adding)
makeRequestDefAddForm fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [ (txt "Name:     " <+>) @@= nonEmptyTextField (name . coerced) RequestDefFormNameField,
        (txt "URL:      " <+>) @@= nonEmptyTextField (url . coerced) RequestDefFormUrlField,
        (txt "Headers:  " <+>) @@= makeKeyValueForm headers HeadersField
      ]
      fs

showAddRequestDefScreen ::
  IxMonadState m => ProjectContext -> m (AppState a) (AppState 'RequestDefAddTag) ()
showAddRequestDefScreen c =
  let fs = RequestDefFormState
        { requestDefFormStateName = RequestDefName "New Request Definition",
          requestDefFormStateUrl = Url "http://example.com",
          requestDefFormStateMethod = Get,
          requestDefFormStateBody = RequestBody "",
          requestDefFormStateHeaders = S.empty
        }
   in imodify $ screen .~ RequestDefAddScreen c (makeRequestDefAddForm fs)
