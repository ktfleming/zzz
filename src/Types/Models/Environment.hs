{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.Environment where

import           Brick                          ( txt )
import           Control.Lens                   ( coerced
                                                , from
                                                , iso
                                                , view
                                                , (^.)
                                                )
import           Control.Lens.TH
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , object
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , (.:)
                                                , (.=)
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Sequence                  ( Seq )
import qualified Data.Text                     as T
import           Types.Classes.Displayable      ( Displayable
                                                , display
                                                )
import           Types.Classes.Fields
import           Types.Models.Id                ( EnvironmentId )
import           Types.Models.KeyValue          ( KeyValue(..)
                                                , KeyValueIso
                                                , keyValueIso
                                                )
import Data.Hashable (Hashable)

newtype EnvironmentName = EnvironmentName T.Text deriving (FromJSON, ToJSON, Eq, Ord)
newtype VariableName = VariableName T.Text deriving (FromJSON, ToJSON, Show, Eq, Hashable)
newtype VariableValue = VariableValue T.Text deriving (FromJSON, ToJSON, Show, Eq)

data Variable = Variable { variableName :: VariableName, variableValue :: VariableValue } deriving (Show, Eq)
makeFields ''Variable

data Environment = Environment { environmentName :: EnvironmentName
                               , environmentVariables :: Seq Variable
                               }

newtype EnvironmentContext = EnvironmentContext EnvironmentId deriving (FromJSON, ToJSON, Show, Eq)
data EnvironmentListItem = NoEnvironment | AnEnvironment EnvironmentContext EnvironmentName

data EnvironmentFormState = EnvironmentFormState { environmentFormStateName :: EnvironmentName
                                                 , environmentFormStateVariables :: Seq Variable
                                                 }

makeFields ''Environment
makeFields ''EnvironmentFormState

instance KeyValueIso Variable where
  keyValueIso = iso
    (\v -> KeyValue (v ^. name . coerced) (v ^. value . coerced))
    (\(KeyValue k v) -> Variable { variableName = VariableName k, variableValue = VariableValue v })

instance ToJSON Environment where
  toJSON e = object ["name" .= (e ^. name . coerced :: T.Text), "variables" .= (e ^. variables)]

instance FromJSON Environment where
  parseJSON = withObject "Environment" $ \o -> Environment <$> (o .: "name") <*> (o .: "variables")

instance Displayable EnvironmentListItem where
  display NoEnvironment       = txt "(No environment)"
  display (AnEnvironment _ n) = txt $ coerce n

instance ToJSON Variable where
  toJSON = toJSON . view keyValueIso

instance FromJSON Variable where
  parseJSON = fmap (view (from keyValueIso)) . parseJSON
