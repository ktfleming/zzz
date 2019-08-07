module UI.List where

import           Brick              (Widget, padRight, txt)
import           Brick.Types        (Padding (Max))
import           Brick.Widgets.List (GenericList, renderList)
import           Data.Vector        (Vector)
import           Types.Classes.Displayable
import           Types.Brick.Name

type ZZZList x = GenericList Name Vector x

renderGenericList :: Displayable x => ZZZList x -> Widget Name
renderGenericList = renderList f True
  where f _ item = padRight Max $ txt $ display item
