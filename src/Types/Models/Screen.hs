module Types.Models.Screen where

import           Types.Models.Project
import           Types.Models.RequestDefinition
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )
import           UI.Projects.Add                ( ProjectAddState )
import           UI.Projects.Edit               ( ProjectEditState )
import           UI.RequestDefinitions.Add      ( RequestDefinitionAddState )
import           UI.RequestDefinitions.Edit     ( RequestDefinitionEditState )

-- Represents what main "view" of the app the user is looking at
data Screen =
  -- Screen name         Required context         Form/list state
  --------------         ----------------         ---------------
    ProjectAddScreen                              (ZZZForm ProjectAddState)
  | ProjectEditScreen    ProjectContext           (ZZZForm ProjectEditState)
  | ProjectListScreen                             (ZZZList ProjectListItem)
  | ProjectDetailsScreen ProjectContext           (ZZZList RequestDefinitionListItem)
  | RequestDetailsScreen RequestDefinitionContext
  | RequestEditScreen    RequestDefinitionContext (ZZZForm RequestDefinitionEditState)
  | RequestAddScreen     ProjectContext           (ZZZForm RequestDefinitionAddState)
  | HelpScreen

instance Show Screen where
  show _ = "(Screen)"
