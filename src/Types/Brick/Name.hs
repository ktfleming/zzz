module Types.Brick.Name where

data Name = ProjectFormNameField
          | ProjectList
          | RequestDefinitionList
          | RequestDefinitionFormNameField
          | RequestDefinitionFormURLField
          | GetRadioField
          | PostRadioField
          | PutRadioField
          | PatchRadioField
          deriving (Eq, Ord, Show)
