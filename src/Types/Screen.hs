module Types.Screen where

import           Types.Project
import           Types.RequestDefinition
import           UI.Form                    (ZZZForm)
import           UI.List                    (ZZZList)
import           UI.Projects.Add            (ProjectAddState)
import           UI.Projects.Edit           (ProjectEditState)
import           UI.RequestDefinitions.Edit (RequestDefinitionEditState)

-- Represents what main "view" of the app the user is looking at
data Screen =
    ProjectAddScreen (ZZZForm ProjectAddState)
  | ProjectEditScreen ProjectContext (ZZZForm ProjectEditState)
  | ProjectListScreen (ZZZList ProjectListItem)
  | ProjectDetailsScreen ProjectContext (ZZZList RequestDefinitionListItem)
  | RequestDetailsScreen RequestDefinitionContext
  | RequestEditScreen RequestDefinitionContext (ZZZForm RequestDefinitionEditState)
  | HelpScreen

instance Show Screen where
  show _ = "(Screen)"
