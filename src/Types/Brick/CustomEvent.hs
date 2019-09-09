module Types.Brick.CustomEvent where

import           Types.Models.Response          ( Response )

import           Types.Models.RequestDef        ( RequestDefContext )

data CustomEvent =
    Save
  | ResponseSuccess RequestDefContext Response
  | ResponseError RequestDefContext String
  | RefreshResponseList -- TODO: delete
