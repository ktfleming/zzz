{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.RequestDefs.Add where

import           Brick                          ( BrickEvent
                                                , EventM
                                                , txt
                                                , (<+>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , handleFormEvent
                                                , newForm
                                                , (@@=)
                                                )
import           Control.Lens
import           Data.Generics.Product.Typed    ( typed )
import           Data.UUID.V4                   ( nextRandom )
import           Types.AppState
import           Types.Brick.Name
import           Types.Methods
import           Types.Models.Id                ( RequestDefId(..) )
import           Types.Models.Project
import           Types.Models.RequestDef
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.Form                        ( ZZZForm )

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State      ( StateT
                                                , modify
                                                )
import           Types.Brick.CustomEvent        ( CustomEvent )

finishAddingRequestDef
  :: MonadIO m => ProjectContext -> RequestDefFormState -> StateT AppState m ()
finishAddingRequestDef (ProjectContext pid) formState = do
  rid <- liftIO $ RequestDefId <$> nextRandom
  let req = RequestDef { requestDefName   = formState ^. name
                       , requestDefUrl    = formState ^. url
                       , requestDefMethod = formState ^. method
                       }
  modify $ projects . at pid . _Just . requestDefs . at rid ?~ req

makeAddRequestDefForm :: ZZZForm RequestDefFormState
makeAddRequestDefForm = newForm
  [ (txt "Request Definition Name: " <+>)
    @@= editTextField (name . coerced) RequestDefFormNameField (Just 1)
  , (txt "URL: " <+>)
    @@= editTextField (url . coerced) RequestDefFormUrlField (Just 1)
  ]
  RequestDefFormState
    { requestDefFormStateName   = RequestDefName "New Request Definition"
    , requestDefFormStateUrl    = Url "http://example.com"
    , requestDefFormStateMethod = Get
    }

showAddRequestDefScreen :: Monad m => ProjectContext -> StateT AppState m ()
showAddRequestDefScreen c =
  modify $ screen .~ RequestDefAddScreen c makeAddRequestDefForm

updateAddRequestDefForm
  :: ZZZForm RequestDefFormState
  -> BrickEvent Name CustomEvent
  -> StateT AppState (EventM Name) ()
updateAddRequestDefForm form ev = do
  updatedForm <- lift $ handleFormEvent ev form
  modify
    $  screen
    .  _RequestDefAddScreen
    .  typed @(ZZZForm RequestDefFormState)
    .~ updatedForm
