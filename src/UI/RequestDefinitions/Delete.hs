{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefinitions.Delete where

import           Control.Lens
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Models.Project           ( requestDefinitions )
import           Types.Models.RequestDefinition

import           Control.Monad.Trans.State      ( StateT
                                                , modify
                                                )

deleteRequestDefinition
  :: Monad m => RequestDefinitionContext -> StateT AppState m ()
deleteRequestDefinition (RequestDefinitionContext pid rid) =
  modify $ projects . at pid . _Just . requestDefinitions . at rid .~ Nothing

deleteRequestDefinitionWarning :: AppState -> RequestDefinitionContext -> T.Text
deleteRequestDefinitionWarning s c =
  let r = lookupRequestDefinition s c
  in  "Are you sure you want to delete request definition '"
        <> r
        ^. name
        .  coerced
        <> "'?"
