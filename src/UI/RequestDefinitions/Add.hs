{-# LANGUAGE TemplateHaskell #-}

module UI.RequestDefinitions.Add where

import qualified Data.Text                     as T
import           Lens.Micro.Platform            ( makeLenses )

data RequestDefinitionAddState = RequestDefinitionAddState { _requestDefinitionAddName :: T.Text } deriving (Show)

makeLenses ''RequestDefinitionAddState
