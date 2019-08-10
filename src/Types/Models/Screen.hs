module Types.Models.Screen where

import           Types.Models.Project
import           Types.Models.RequestDefinition
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )

-- Represents what main "view" of the app the user is looking at
data Screen =
  -- Screen name         Required context         Form/list state
  --------------         ----------------         ---------------
    ProjectAddScreen                              (ZZZForm ProjectFormState)
  | ProjectEditScreen    ProjectContext           (ZZZForm ProjectFormState)
  | ProjectListScreen                             (ZZZList ProjectListItem)
  | ProjectDetailsScreen ProjectContext           (ZZZList RequestDefinitionListItem)
  | RequestDetailsScreen RequestDefinitionContext
  | RequestEditScreen    RequestDefinitionContext (ZZZForm RequestDefinitionFormState)
  | RequestAddScreen     ProjectContext           (ZZZForm RequestDefinitionFormState)
  | HelpScreen
  | ConsoleScreen

instance Show Screen where
  show _ = "(Screen)"
