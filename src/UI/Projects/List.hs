module UI.Projects.List where

import           Brick                          ( Widget
                                                , txt
                                                )
import           Brick.Widgets.List             ( GenericList
                                                , renderList
                                                , list
                                                )
import           Data.List
import           Data.Vector                    ( Vector
                                                , fromList
                                                )
import           Types.Name
import           Types.Project

renderProjectList :: [Project] -> Widget Name
renderProjectList ps = renderList renderFn True theList
 where
  renderFn :: Bool -> Project -> Widget Name
  renderFn _ p = txt $ projectName p
  theList :: GenericList Name Vector Project
  theList = list ProjectList (fromList ps) 1
