module Types.Brick.CustomEvent where

import Types.Models.RequestDef (RequestDefContext)
import Types.Models.Response (Response)

data CustomEvent
  = Save
  | ResponseSuccess RequestDefContext Response
  | ResponseError RequestDefContext String
