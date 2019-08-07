module Types.Brick.Name where

data Name = ProjectAddNameField
          | ProjectEditNameField
          | ProjectList
          | RequestDefinitionList
          | RequestDefinitionNameEditField
          | RequestDefinitionNameAddField
          deriving (Eq, Ord, Show)
