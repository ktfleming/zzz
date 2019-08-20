{-# LANGUAGE OverloadedStrings #-}

module UI.RequestDefs.Delete where

import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , imodify
                                                )
import qualified Data.Text                     as T
import           Types.AppState
import           Types.Models.Project           ( requestDefs )
import           Types.Models.RequestDef

deleteRequestDef :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState a) ()
deleteRequestDef (RequestDefContext pid rid) =
  imodify $ projects . at pid . _Just . requestDefs . at rid .~ Nothing

deleteRequestDefWarning :: AppState a -> RequestDefContext -> T.Text
deleteRequestDefWarning s c =
  let r = lookupRequestDef s c
  in  "Are you sure you want to delete request definition '" <> r ^. name . coerced <> "'?"
