module Types.ContextTransformers where

import           Control.Lens
import           Types.Classes.Fields
import           Types.Models.Id                ( RequestDefId )
import           Types.Models.Project           ( ProjectContext(..) )
import           Types.Models.RequestDef

requestDefContext :: ProjectContext -> RequestDefId -> RequestDefContext
requestDefContext (ProjectContext pid) = RequestDefContext pid

requestDefListItem :: RequestDefContext -> RequestDef -> RequestDefListItem
requestDefListItem c r = RequestDefListItem c (r ^. name)
