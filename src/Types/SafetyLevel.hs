{-# LANGUAGE OverloadedStrings #-}

module Types.SafetyLevel where

import Control.Applicative (empty)
import Data.Aeson
import Data.Text (Text)
import Types.Brick.Name
import Types.Methods

data SafetyLevel
  = NeverPrompt
  | PromptForPossiblyUnsafe -- prompt for POST, PUT, PATCH, DELETE
  | AlwaysPrompt
  deriving (Eq, Show)

-- The "greatest" SafetyLevel is the most restrictive
instance Ord SafetyLevel where
  NeverPrompt <= _ = True
  PromptForPossiblyUnsafe <= NeverPrompt = False
  PromptForPossiblyUnsafe <= _ = True
  AlwaysPrompt <= AlwaysPrompt = True
  AlwaysPrompt <= _ = False

instance ToJSON SafetyLevel where
  toJSON NeverPrompt = String "never_prompt"
  toJSON PromptForPossiblyUnsafe = String "prompt_for_possibly_unsafe"
  toJSON AlwaysPrompt = String "always_prompt"

instance FromJSON SafetyLevel where
  parseJSON (String "never_prompt") = pure NeverPrompt
  parseJSON (String "prompt_for_possibly_unsafe") = pure PromptForPossiblyUnsafe
  parseJSON (String "always_prompt") = pure AlwaysPrompt
  parseJSON _ = empty

allSafetyLevelsRadio :: [(SafetyLevel, Name, Text)]
allSafetyLevelsRadio =
  [ (NeverPrompt, NeverPromptRadioField, "Never prompt"),
    (PromptForPossiblyUnsafe, PromptForPossiblyUnsafeRadioField, "Prompt for possibly unsafe methods (POST, PUT, PATCH, DELETE)"),
    (AlwaysPrompt, AlwaysPromptRadioField, "Always prompt")
  ]

shouldPrompt :: SafetyLevel -> Method -> Bool
shouldPrompt NeverPrompt _ = False
shouldPrompt PromptForPossiblyUnsafe level = elem level [Post, Put, Patch, Delete]
shouldPrompt AlwaysPrompt _ = True
