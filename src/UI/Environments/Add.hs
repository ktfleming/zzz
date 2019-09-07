{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Environments.Add
  ( finishAddingEnvironment
  , showEnvironmentAddScreen
  )
where

import           Brick                          ( txt
                                                , (<+>)
                                                , (<=>)
                                                )
import           Brick.Forms                    ( editTextField
                                                , formState
                                                , newForm
                                                , setFormConcat
                                                , (@@=)
                                                )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Sequence                 as S
import           Data.UUID.V4                   ( nextRandom )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Models.Environment       ( Environment(..)
                                                , EnvironmentFormState(..)
                                                , EnvironmentName(..)
                                                )
import           Types.Models.Id                ( EnvironmentId(..) )
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm
                                                , spacedConcat
                                                )
import           UI.Forms.KeyValueList          ( makeKeyValueForm )

finishAddingEnvironment
  :: MonadIO m => IxStateT m (AppState 'EnvironmentAddTag) (AppState 'EnvironmentAddTag) ()
finishAddingEnvironment = do
  s <- iget
  let EnvironmentAddScreen form = s ^. screen
  eid <- liftIO $ EnvironmentId <$> nextRandom
  let e = Environment { environmentName      = formState form ^. name
                      , environmentVariables = formState form ^. variables
                      }
  imodify $ environments . at eid ?~ e

makeEnvironmentAddForm :: ZZZForm EnvironmentFormState
makeEnvironmentAddForm = setFormConcat spacedConcat $ newForm
  [ (txt "Environment Name: " <+>)
    @@= editTextField (name . coerced) EnvironmentFormNameField (Just 1)
  , (txt "Variables:" <=>) @@= makeKeyValueForm variables VariablesField
  ]
  EnvironmentFormState { environmentFormStateName      = EnvironmentName "New Environment"
                       , environmentFormStateVariables = S.empty
                       }

showEnvironmentAddScreen :: Monad m => IxStateT m (AppState a) (AppState 'EnvironmentAddTag) ()
showEnvironmentAddScreen = imodify $ screen .~ EnvironmentAddScreen makeEnvironmentAddForm
