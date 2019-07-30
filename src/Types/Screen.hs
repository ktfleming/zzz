module Types.Screen where

import           Types.Project

-- Represents what main "view" of the app the user is looking at
data Screen = ProjectDetailsScreen Project | ProjectListScreen | HelpScreen deriving (Show)
