module Types.Name where

data Name = ProjectAddNameField
          | ProjectEditNameField
          | ProjectList
          | RequestDefinitionList
          | RequestDefinitionNameField
          deriving (Eq, Ord, Show)
