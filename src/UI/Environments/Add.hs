{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Environments.Add
  ( finishAddingEnvironment,
    showEnvironmentAddScreen,
    makeEnvironmentAddForm,
  )
where

import Brick
  ( (<+>),
    (<=>),
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
import Types.Models.Environment
  ( Environment (..),
    EnvironmentFormState (..),
    EnvironmentName (..),
  )
import Types.Models.Id (EnvironmentId (..))
import Types.Models.Screen
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

finishAddingEnvironment ::
  IxMonadState m =>
  EnvironmentId ->
  m (AppState 'EnvironmentAddTag) (AppState 'EnvironmentAddTag) ()
finishAddingEnvironment eid = do
  s <- iget
  let EnvironmentAddScreen (AppForm form) = s ^. screen
  let e = Environment
        { environmentName = formState form ^. name,
          environmentVariables = formState form ^. variables
        }
  imodify $ environments . at eid ?~ e

makeEnvironmentAddForm :: EnvironmentFormState 'Adding -> AppForm (EnvironmentFormState 'Adding)
makeEnvironmentAddForm fs =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [ (txt "Environment Name: " <+>)
          @@= nonEmptyTextField (name . coerced) EnvironmentFormNameField,
        (txt "Variables:" <=>) @@= makeKeyValueForm variables VariablesField
      ]
      fs

showEnvironmentAddScreen :: IxMonadState m => m (AppState a) (AppState 'EnvironmentAddTag) ()
showEnvironmentAddScreen =
  let fs = EnvironmentFormState
        { environmentFormStateName = EnvironmentName "New Environment",
          environmentFormStateVariables = S.empty
        }
   in imodify $ screen .~ EnvironmentAddScreen (makeEnvironmentAddForm fs)
