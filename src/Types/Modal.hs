{-# LANGUAGE OverloadedStrings #-}

module Types.Modal where

import Brick
import Brick.Forms
import Control.Lens
import Types.Brick.Name
import Types.Models.Environment
import Types.Models.Project (ProjectContext)
import Types.Models.RequestDef (RequestDefContext)
import Types.Models.Response (ResponseIndex)
import UI.Form

-- current name / current value / remaining names that need to be set
data VariablePromptFormState = VariablePromptFormState VariableName VariableValue [VariableName] deriving (Show, Eq)

data Modal
  = DeleteProjectModal ProjectContext
  | DeleteRequestDefModal RequestDefContext
  | DeleteEnvironmentModal EnvironmentContext
  | DeleteResponseModal RequestDefContext ResponseIndex
  | ConfirmRequestModal RequestDefContext
  | VariablePromptModal RequestDefContext (AppForm VariablePromptFormState) Bool
  deriving (Show, Eq)

-- The next Modal to show after the current one has been confirmed
nextModal :: Modal -> Maybe Modal
nextModal (VariablePromptModal c (AppForm fs) needsPrompt) =
  case (formState fs, needsPrompt) of
    (VariablePromptFormState _ _ (x : xs), _) ->
      -- Have to present a modal for each unbound variable
      Just $ VariablePromptModal c (variablePromptForm (VariablePromptFormState x (VariableValue "") xs)) needsPrompt
    (VariablePromptFormState _ _ [], True) ->
      -- Final confirmation modal
      Just $ ConfirmRequestModal c
    (VariablePromptFormState _ _ [], False) ->
      Nothing
nextModal _ = Nothing

variablePromptForm :: VariablePromptFormState -> AppForm VariablePromptFormState
variablePromptForm fs@(VariablePromptFormState (VariableName n) _ _) =
  AppForm $ setFormConcat spacedConcat $
    newForm
      [(txt ("Enter value for " <> n <> ": ") <+>) @@= editTextField (variableValueLens . coerced) VariableValueField (Just 1)]
      fs

-- Lens into the one part of the state that gets updated via a Brick editor
variableValueLens :: Lens' VariablePromptFormState VariableValue
variableValueLens = lens (\(VariablePromptFormState _ v _) -> v) (\(VariablePromptFormState n _ vs) v -> VariablePromptFormState n v vs)
