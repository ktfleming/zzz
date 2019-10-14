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
  )
where

import Brick
  ( (<+>),
    (<=>),
    txt,
  )
import Brick.Forms
  ( (@@=),
    editTextField,
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

makeEnvironmentAddForm :: AppForm (EnvironmentFormState 'Adding)
makeEnvironmentAddForm = AppForm $ setFormConcat spacedConcat $ newForm
  [ (txt "Environment Name: " <+>)
      @@= editTextField (name . coerced) EnvironmentFormNameField (Just 1),
    (txt "Variables:" <=>) @@= makeKeyValueForm variables VariablesField
  ]
  EnvironmentFormState
    { environmentFormStateName = EnvironmentName "New Environment",
      environmentFormStateVariables = S.empty
    }

showEnvironmentAddScreen :: IxMonadState m => m (AppState a) (AppState 'EnvironmentAddTag) ()
showEnvironmentAddScreen = imodify $ screen .~ EnvironmentAddScreen makeEnvironmentAddForm
