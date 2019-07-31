module Types.Screen where

import           Types.Project
import           UI.List                        ( ZZZList )
import           UI.Form                        ( ZZZForm )
import           UI.Projects.Add                ( ProjectAddState )

data ProjectListSubscreen = AddingProject (ZZZForm ProjectAddState) | ListingProjects (ZZZList Project)

instance Show ProjectListSubscreen where
  show (AddingProject   _) = "AddingProject"
  show (ListingProjects _) = "ListingProjects"

-- Represents what main "view" of the app the user is looking at
data Screen =
    ProjectDetailsScreen Project
  | ProjectListScreen ProjectListSubscreen
  | HelpScreen deriving (Show)
