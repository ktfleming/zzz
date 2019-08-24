module Types.Brick.Name where

data Name =
          -- Project list screen
            ProjectList

          -- Project add/edit screen
          | ProjectFormNameField

          -- Project details screen
          | RequestDefList

          -- Request details screen
          | ResponseList
          | ResponseBodyDetails
          | ResponseBodyViewport

          -- Request add/edit screen
          | RequestDefFormNameField
          | RequestDefFormUrlField
          | GetRadioField
          | PostRadioField
          | PutRadioField
          | PatchRadioField
          | HeadersField
          | RequestBodyField
          deriving (Eq, Ord, Show)
