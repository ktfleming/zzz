{-# LANGUAGE OverloadedStrings #-}

module Types.Brick.Name where

import Data.Text (Text)

data Name
  = -- Project list screen
    ProjectList
  | -- Project add/edit screen
    ProjectFormNameField
  | -- Project details screen
    RequestDefList
  | -- Request details screen (focus ring)
    RequestDetails
  | ResponseList
  | ResponseBodyDetails
  | -- Request details screen (viewport)
    ResponseBodyViewport
  | -- Request add/edit screen
    RequestDefFormNameField
  | RequestDefFormUrlField
  | GetRadioField
  | PostRadioField
  | PutRadioField
  | PatchRadioField
  | DeleteRadioField
  | HeadersField
  | RequestBodyField
  | -- Environment list screen
    EnvironmentList
  | -- Environment add/edit screen
    EnvironmentFormNameField
  | VariablesField
  | NeverPromptRadioField
  | PromptForPossiblyUnsafeRadioField
  | AlwaysPromptRadioField
  | -- Search screen
    SearchField
  | SearchResultsList
  | -- Variable prompt modal
    VariableValueField
  deriving (Eq, Ord, Show)

-- The label to display for a Name that's used as as form input
-- (used to notify the user of which fields are invalid)
label :: Name -> Maybe Text
label ProjectFormNameField = Just "Project Name"
label RequestDefFormNameField = Just "Name"
label RequestDefFormUrlField = Just "URL"
label HeadersField = Just "Headers"
label RequestBodyField = Just "Body"
label _ = Nothing
