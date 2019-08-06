module UI.Projects.List where

import           Brick.Widgets.List        (list)
import           Data.Vector               (fromList)
import           Types.ContextTransformers (projectListItem)
import           Types.Name
import           Types.Project
import           UI.List                   (ZZZList)

makeProjectList :: [Project] -> ZZZList ProjectListItem
makeProjectList ps =
  let listItems = fmap projectListItem (fromList ps)
  in list ProjectList listItems 1
