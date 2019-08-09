module Types.Brick.Name where

data Name = ProjectAddNameField
          | ProjectEditNameField
          | ProjectList
          | RequestDefinitionList
          | RequestDefinitionNameEditField
          | RequestDefinitionURLEditField
          | RequestDefinitionNameAddField
          | RequestDefinitionURLAddField
          | GetRadioField
          | PostRadioField
          | PutRadioField
          | PatchRadioField
          deriving (Eq, Ord, Show)
