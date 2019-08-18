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
          | ResponseBody
          | ResponseBodyViewport

          -- Request add/edit screen
          | RequestDefFormNameField
          | RequestDefFormUrlField
          | GetRadioField
          | PostRadioField
          | PutRadioField
          | PatchRadioField
          deriving (Eq, Ord, Show)
