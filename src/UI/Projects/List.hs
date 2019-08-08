{-# LANGUAGE NamedFieldPuns #-}
module UI.Projects.List where

import           Brick.Widgets.List        (list)
import           Types.Brick.Name
import           Types.Models.Project
import           UI.List                   (ZZZList)
import Types.Models.ID (ProjectID)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- This is defined here and not in `Listable` because it's also used to generate the
-- initial AppState; needs to be here to avoid cyclic imports.
makeProjectList :: Map ProjectID Project -> ZZZList ProjectListItem
makeProjectList pm =
  let tuples = (V.fromList . Map.toList) pm
      listItems = fmap (\(pid, Project { _projectName }) -> ProjectListItem (ProjectContext pid) _projectName) tuples
  in list ProjectList listItems 1