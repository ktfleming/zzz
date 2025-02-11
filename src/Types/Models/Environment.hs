{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Models.Environment where

import Control.Lens
  ( (^.),
    coerced,
    from,
    iso,
    view,
  )
import Control.Lens.TH
import Data.Aeson
  ( (.:),
    (.=),
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
  )
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Classes.Fields
import Types.Models.Id (EnvironmentId)
import Types.Models.KeyValue
  ( KeyValue (..),
    KeyValueIso,
    keyValueIso,
  )
import Types.SafetyLevel

newtype EnvironmentName = EnvironmentName Text deriving (FromJSON, ToJSON, Eq, Ord, Show)

newtype VariableName = VariableName Text deriving (FromJSON, ToJSON, Show, Eq, Ord, Generic)

newtype VariableValue = VariableValue Text deriving (FromJSON, ToJSON, Show, Eq, Generic)

data Variable = Variable {variableName :: VariableName, variableValue :: VariableValue} deriving (Show, Eq, Generic)

instance Hashable VariableName

instance Hashable VariableValue

instance Hashable Variable

makeFields ''Variable

data Environment
  = Environment
      { environmentName :: EnvironmentName,
        environmentVariables :: Seq Variable,
        environmentSafetyLevel :: SafetyLevel
      }
  deriving (Eq, Show)

newtype EnvironmentContext = EnvironmentContext EnvironmentId deriving (FromJSON, ToJSON, Show, Eq)

data EnvironmentFormState
  = EnvironmentFormState
      { environmentFormStateName :: EnvironmentName,
        environmentFormStateVariables :: Seq Variable,
        environmentFormStateSafetyLevel :: SafetyLevel
      }
  deriving (Eq, Show)

makeFields ''Environment

makeFields ''EnvironmentFormState

instance KeyValueIso Variable where
  keyValueIso =
    iso
      (\v -> KeyValue (v ^. name . coerced) (v ^. value . coerced))
      (\(KeyValue k v) -> Variable {variableName = VariableName k, variableValue = VariableValue v})

instance ToJSON Environment where
  toJSON e =
    object
      [ "name" .= (e ^. name . coerced :: Text),
        "variables" .= (e ^. variables),
        "safety_level" .= (e ^. safetyLevel)
      ]

instance FromJSON Environment where
  parseJSON = withObject "Environment" $ \o -> Environment <$> (o .: "name") <*> (o .: "variables") <*> (o .: "safety_level")

instance ToJSON Variable where
  toJSON = toJSON . view keyValueIso

instance FromJSON Variable where
  parseJSON = fmap (view (from keyValueIso)) . parseJSON
