{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.Environments.Edit
  ( finishEditingEnvironment
  , showEnvironmentEditScreen
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
import           Data.String                    ( fromString )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Models.Environment
import           Types.Models.Screen
import           UI.Form                        ( ZZZForm
                                                , spacedConcat
                                                )
import           UI.Forms.KeyValueList          ( makeKeyValueForm )

finishEditingEnvironment
  :: Monad m => IxStateT m (AppState 'EnvironmentEditTag) (AppState 'EnvironmentEditTag) ()
finishEditingEnvironment = do
  s <- iget
  let EnvironmentEditScreen c@(EnvironmentContext eid) form = s ^. screen
      newModel = updateEnvironment (model s c) (formState form)
  imodify $ environments . ix eid .~ newModel

updateEnvironment :: Environment -> EnvironmentFormState -> Environment
updateEnvironment base form = base & name .~ (form ^. name) & variables .~ (form ^. variables)

makeEnvironmentEditForm :: AppState a -> EnvironmentContext -> ZZZForm EnvironmentFormState
makeEnvironmentEditForm s c =
  let e         = model s c
      editState = EnvironmentFormState { environmentFormStateName      = e ^. name
                                       , environmentFormStateVariables = e ^. variables
                                       }
  in  setFormConcat spacedConcat $ newForm
        [ (txt "Environment Name: " <+>)
          @@= editTextField (name . coerced) EnvironmentFormNameField (Just 1)
        , (txt "Variables:" <=>) @@= makeKeyValueForm variables VariablesField
        ]
        editState

showEnvironmentEditScreen
  :: Monad m => EnvironmentContext -> IxStateT m (AppState a) (AppState 'EnvironmentEditTag) ()
showEnvironmentEditScreen c =
  imodify $ \s -> s & screen .~ EnvironmentEditScreen c (makeEnvironmentEditForm s c)
