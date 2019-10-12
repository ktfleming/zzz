{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.Models.RequestDef where

import           Brick                          ( txt )
import           Control.Lens                   ( coerced
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
import           Data.Maybe                     ( catMaybes )
import           Data.Sequence                  ( Seq )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Parsing.TemplatedUrlParser     ( TemplatedUrl(..)
                                                , TemplatedUrlPart(..)
                                                , parseTemplatedUrl
                                                )
import           Text.Megaparsec                ( runParser )
import           Types.Classes.Displayable
import           Types.Classes.Fields
import           Types.Methods                  ( Method )
import           Types.Models.Environment       ( VariableName(..) )
import           Types.Models.Header
import           Types.Models.Id                ( ProjectId
                                                , RequestDefId
                                                )
import           Types.Models.Url               ( Url(..) )

newtype RequestDefName = RequestDefName T.Text deriving (FromJSON, ToJSON, Show, Eq, Ord)
newtype RequestBody = RequestBody T.Text deriving (FromJSON, ToJSON, Show, Eq)

data RequestError =
    RequestFailed UTCTime T.Text       -- tried to sent request, but failed (could not parse URL, etc)
  | UnmatchedVariables [VariableName]  -- URL/headers/body contains {{variables}} that aren't defined in the current environment
  deriving (Show)

errorDescription :: RequestError -> T.Text
errorDescription (RequestFailed errorTime msg) =
  "The request sent at " <> (T.pack . formatISO8601) errorTime <> " failed: " <> msg
errorDescription (UnmatchedVariables vars) =
  "The following variables are not defined in the current environment: "
    <> (T.intercalate ", " (coerce vars))

data RequestDef = RequestDef {
    requestDefName :: RequestDefName
  , requestDefUrl :: Url
  , requestDefMethod :: Method
  , requestDefBody :: RequestBody
  , requestDefHeaders :: Seq Header
  } deriving (Show)

data RequestDefFormState = RequestDefFormState {
    requestDefFormStateName :: RequestDefName
  , requestDefFormStateUrl :: Url
  , requestDefFormStateMethod :: Method
  , requestDefFormStateBody :: RequestBody
  , requestDefFormStateHeaders :: Seq Header
  } deriving (Show)

data RequestDefContext = RequestDefContext ProjectId RequestDefId deriving (Show)

data RequestDefListItem = RequestDefListItem RequestDefContext RequestDefName

makeFields ''RequestDef
makeFields ''RequestDefFormState

-- Extract all of the template variables used within a RequestDef, including in the URL,
-- headers, body, etc.
allVariables :: RequestDef -> [VariableName]
allVariables r =
  let
    transformPart :: TemplatedUrlPart -> Maybe VariableName
    transformPart (TemplateVariable n) = Just (VariableName n)
    transformPart (TextPart         _) = Nothing

    -- This plus `transformPart` is like Scala's `collect`, i.e. keep the VariableNames and
    -- discard the other parts
    getVariables :: TemplatedUrl -> [VariableName]
    getVariables (TemplatedUrl ps) = catMaybes $ transformPart <$> ps

    urlVariables = either (const [])
                          getVariables
                          (runParser parseTemplatedUrl "Templated URL" (r ^. url . coerced))
  in
    urlVariables

instance Displayable RequestDefListItem where
  display (RequestDefListItem _ n) = txt $ coerce n

instance ToJSON RequestDef where
  toJSON r = object
    [ "name" .= (r ^. name . coerced :: T.Text)
    , "url" .= (r ^. url . coerced :: T.Text)
    , "method" .= (r ^. method)
    , "body" .= (r ^. body)
    , "headers" .= (r ^. headers)
    ]

instance FromJSON RequestDef where
  parseJSON = withObject "RequestDef" $ \o ->
    RequestDef
      <$> (o .: "name")
      <*> (o .: "url")
      <*> (o .: "method")
      <*> (o .: "body")
      <*> (o .: "headers")
