{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.RequestDefinitions.Add where

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
import           Types.Models.Id                ( RequestDefinitionId(..) )
import           Types.Models.Project
import           Types.Models.RequestDefinition
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

finishAddingRequestDefinition
  :: MonadIO m
  => ProjectContext
  -> RequestDefinitionFormState
  -> StateT AppState m ()
finishAddingRequestDefinition (ProjectContext pid) formState = do
  rid <- liftIO $ RequestDefinitionId <$> nextRandom
  let req = RequestDefinition { requestDefinitionName   = formState ^. name
                              , requestDefinitionUrl    = formState ^. url
                              , requestDefinitionMethod = formState ^. method
                              }
  modify $ projects . at pid . _Just . requestDefinitions . at rid ?~ req

makeAddRequestDefinitionForm :: ZZZForm RequestDefinitionFormState
makeAddRequestDefinitionForm = newForm
  [ (txt "Request Definition Name: " <+>)
    @@= editTextField (name . coerced) RequestDefinitionFormNameField (Just 1)
  , (txt "URL: " <+>)
    @@= editTextField (url . coerced) RequestDefinitionFormUrlField (Just 1)
  ]
  RequestDefinitionFormState
    { requestDefinitionFormStateName   = RDName "New Request Definition"
    , requestDefinitionFormStateUrl    = Url "http://example.com"
    , requestDefinitionFormStateMethod = Get
    }

showAddRequestDefinitionScreen
  :: Monad m => ProjectContext -> StateT AppState m ()
showAddRequestDefinitionScreen c =
  modify $ screen .~ RequestAddScreen c makeAddRequestDefinitionForm

updateAddRequestDefinitionForm
  :: ZZZForm RequestDefinitionFormState
  -> BrickEvent Name CustomEvent
  -> StateT AppState (EventM Name) ()
updateAddRequestDefinitionForm form ev = do
  updatedForm <- lift $ handleFormEvent ev form
  modify
    $  screen
    .  _RequestAddScreen
    .  typed @(ZZZForm RequestDefinitionFormState)
    .~ updatedForm
