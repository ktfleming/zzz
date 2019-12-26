{-# LANGUAGE OverloadedStrings #-}

module UI.Responses.Delete
  ( deleteResponse,
    deleteResponseWarning,
  )
where

import Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Types.AppState
import Types.Models.RequestDef
import Types.Models.Response (ResponseIndex (..))

deleteResponse ::
  RequestDefContext -> ResponseIndex -> AppState a -> AppState a
deleteResponse (RequestDefContext _ rid) (ResponseIndex i) s =
  s & responses . at rid . _Just . ix (currentEnvironmentKey s) %~ Seq.deleteAt i

deleteResponseWarning :: T.Text
deleteResponseWarning = "Are you sure you want to delete this response?"
