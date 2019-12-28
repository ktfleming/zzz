{-# LANGUAGE DataKinds #-}
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
import qualified Data.Text as T
import Types.Classes.Fields
import Types.Models.Id (EnvironmentId)
import Types.Models.KeyValue
  ( KeyValue (..),
    KeyValueIso,
    keyValueIso,
  )

newtype EnvironmentName = EnvironmentName T.Text deriving (FromJSON, ToJSON, Eq, Ord, Show)

newtype VariableName = VariableName T.Text deriving (FromJSON, ToJSON, Show, Eq, Hashable)

newtype VariableValue = VariableValue T.Text deriving (FromJSON, ToJSON, Show, Eq)

data Variable = Variable {variableName :: VariableName, variableValue :: VariableValue} deriving (Show, Eq)

makeFields ''Variable

data Environment
  = Environment
      { environmentName :: EnvironmentName,
        environmentVariables :: Seq Variable
      }
  deriving (Eq, Show)

newtype EnvironmentContext = EnvironmentContext EnvironmentId deriving (FromJSON, ToJSON, Show, Eq)

data EnvironmentFormState
  = EnvironmentFormState
      { environmentFormStateName :: EnvironmentName,
        environmentFormStateVariables :: Seq Variable
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
  toJSON e = object ["name" .= (e ^. name . coerced :: T.Text), "variables" .= (e ^. variables)]

instance FromJSON Environment where
  parseJSON = withObject "Environment" $ \o -> Environment <$> (o .: "name") <*> (o .: "variables")

instance ToJSON Variable where
  toJSON = toJSON . view keyValueIso

instance FromJSON Variable where
  parseJSON = fmap (view (from keyValueIso)) . parseJSON
