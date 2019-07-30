module UI.Projects.List where

import           Brick.Widgets.List             ( GenericList
                                                , list
                                                )
import           Data.List
import           Data.Vector                    ( Vector
                                                , fromList
                                                )
import           Types.Name
import           Types.Project
import           UI.List                        ( ZZZList )

makeProjectList :: [Project] -> ZZZList Project
makeProjectList ps = list ProjectList (fromList ps) 1
