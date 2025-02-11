{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Models.RequestDef where

import Control.Lens
  ( (^.),
    coerced,
  )
import Control.Lens.TH
import Control.Monad (join)
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
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Format.ISO8601
import Parsing.TemplatedTextParser
  ( TemplatedText (..),
    TemplatedTextPart (..),
    parseTemplatedText,
  )
import Text.Megaparsec (runParser)
import Types.Classes.Fields
import Types.Methods (Method)
import Types.Models.Environment (VariableName (..))
import Types.Models.Header
import Types.Models.Id
  ( ProjectId,
    RequestDefId,
  )
import Types.Models.Url (Url (..))
import Types.Time

newtype RequestDefName = RequestDefName Text deriving (FromJSON, ToJSON, Show, Eq, Ord)

newtype RequestBody = RequestBody Text deriving (FromJSON, ToJSON, Show, Eq)

data RequestError
  = RequestFailed AppTime Text -- tried to sent request, but failed (could not parse URL, etc)
  | UnmatchedVariables [VariableName] -- URL/headers/body contains {{variables}} that aren't defined in the current environment
  deriving (Show, Eq)

errorDescription :: RequestError -> Text
errorDescription (RequestFailed (AppTime errorTime) msg) =
  "The request sent at " <> (T.pack . iso8601Show) errorTime <> " failed: " <> msg
errorDescription (UnmatchedVariables vars) =
  "The following variables are not defined in the current environment: "
    <> T.intercalate ", " (coerce vars)

data RequestDef
  = RequestDef
      { requestDefName :: RequestDefName,
        requestDefUrl :: Url,
        requestDefMethod :: Method,
        requestDefBody :: RequestBody,
        requestDefHeaders :: Seq Header
      }
  deriving (Show, Eq)

data RequestDefFormState
  = RequestDefFormState
      { requestDefFormStateName :: RequestDefName,
        requestDefFormStateUrl :: Url,
        requestDefFormStateMethod :: Method,
        requestDefFormStateBody :: RequestBody,
        requestDefFormStateHeaders :: Seq Header
      }
  deriving (Show, Eq)

data RequestDefContext = RequestDefContext ProjectId RequestDefId deriving (Show, Eq)

makeFields ''RequestDef

makeFields ''RequestDefFormState

-- Extract all of the template variables used within a RequestDef, including in the URL,
-- headers, body, etc.
allVariables :: RequestDef -> HashSet VariableName
allVariables r =
  let transformPart :: TemplatedTextPart -> Maybe VariableName
      transformPart (TemplateVariable n) = Just (VariableName n)
      transformPart (TextPart _) = Nothing
      -- This plus `transformPart` is like Scala's `collect`, i.e. keep the VariableNames and
      -- discard the other parts
      collectVariables :: TemplatedText -> [VariableName]
      collectVariables (TemplatedText ps) = catMaybes $ transformPart <$> ps
      -- Run the parser on the URL, headers, and body to extract variables that are used inside it
      extractVariables :: Text -> [VariableName]
      extractVariables t = either (const []) collectVariables (runParser parseTemplatedText "Templated text" t)
      urlVariables = extractVariables $ r ^. url . coerced
      bodyVariables = extractVariables $ r ^. body . coerced
      headerVariables = toList (r ^. headers) >>= (\(Header n v) -> join [extractVariables (coerce n), extractVariables (coerce v)])
   in HashSet.fromList $ join [urlVariables, bodyVariables, headerVariables]

instance ToJSON RequestDef where
  toJSON r =
    object
      [ "name" .= (r ^. name . coerced :: Text),
        "url" .= (r ^. url . coerced :: Text),
        "method" .= (r ^. method),
        "body" .= (r ^. body),
        "headers" .= (r ^. headers)
      ]

instance FromJSON RequestDef where
  parseJSON = withObject "RequestDef" $ \o ->
    RequestDef
      <$> (o .: "name")
      <*> (o .: "url")
      <*> (o .: "method")
      <*> (o .: "body")
      <*> (o .: "headers")
