{-# LANGUAGE OverloadedStrings #-}

module UI.Responses.Delete
  ( deleteResponse
  , deleteResponseWarning
  )
where

import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , imodify
                                                )
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Models.RequestDef
import           Types.Models.Response          ( ResponseIndex(..) )

deleteResponse
  :: Monad m => RequestDefContext -> ResponseIndex -> IxStateT m (AppState a) (AppState a) ()
deleteResponse (RequestDefContext _ rid) (ResponseIndex i) =
  imodify $ responses . ix rid %~ S.deleteAt i

deleteResponseWarning :: T.Text
deleteResponseWarning = "Are you sure you want to delete this response?"
