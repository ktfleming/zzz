module UI.Projects.List where

import           Brick                          ( withAttr
                                                , Widget
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
import           UI.Attr

renderProjectList :: [Project] -> Widget Name
renderProjectList ps = renderList renderFn True theList
 where
  renderFn :: Bool -> Project -> Widget Name
  renderFn active p =
    let baseWidget = txt $ projectName p
    in  if active then withAttr highlighted baseWidget else baseWidget
  theList :: GenericList Name Vector Project
  theList = list ProjectList (fromList ps) 1
