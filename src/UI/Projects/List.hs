module UI.Projects.List where

import           Brick.Widgets.List             ( list )
import           Data.Vector                    ( fromList )
import           Types.Name
import           Types.Project
import           UI.List                        ( ZZZList )

makeProjectList :: [Project] -> ZZZList Project
makeProjectList ps = list ProjectList (fromList ps) 1
