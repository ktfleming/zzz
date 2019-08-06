{-# LANGUAGE TemplateHaskell #-}

module UI.RequestDefinitions.Edit where

import qualified Data.Text                     as T
import           Lens.Micro.Platform            ( makeLenses )

data RequestDefinitionEditState = RequestDefinitionEditState { _requestDefinitionEditName :: T.Text }

makeLenses ''RequestDefinitionEditState
