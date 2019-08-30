module Types.Brick.Name where

data Name =
          -- Project list screen
            ProjectList

          -- Project add/edit screen
          | ProjectFormNameField

          -- Project details screen
          | RequestDefList

          -- Request details screen (focus ring)
          | RequestDetails
          | ResponseList
          | ResponseBodyDetails

          -- Request details screen (viewport)
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

          -- Search screen
          | SearchField
          | SearchResultsList

          deriving (Eq, Ord, Show)
