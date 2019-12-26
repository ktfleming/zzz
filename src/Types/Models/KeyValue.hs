{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Models.KeyValue where

import Control.Lens hiding ((.=))
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
import Data.Text as T
import Types.Classes.Fields

data KeyValue = KeyValue {keyValueName :: T.Text, keyValueValue :: T.Text}

makeFields ''KeyValue

isKeyValueEnabled :: KeyValue -> Bool
isKeyValueEnabled kv = isTextEnabled $ kv ^. name . coerced

-- If a key/value pair's name starts with "--", this means that it should be disabled ("commented
-- out", so to speak).
isTextEnabled :: T.Text -> Bool
isTextEnabled t = T.take 2 t /= "--"

class KeyValueIso a where

  keyValueIso :: Iso' a KeyValue

  isEnabled :: a -> Bool
  isEnabled = isKeyValueEnabled . view keyValueIso

instance ToJSON KeyValue where
  toJSON kv = object ["name" .= (kv ^. name), "value" .= (kv ^. value)]

instance FromJSON KeyValue where
  parseJSON = withObject "KeyValue" $ \o -> KeyValue <$> (o .: "name") <*> (o .: "value")
