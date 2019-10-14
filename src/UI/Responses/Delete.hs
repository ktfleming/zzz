{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Responses.Delete
  ( deleteResponse,
    deleteResponseWarning,
  )
where

import Control.Lens
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import qualified Data.Sequence as S
import Data.String (fromString)
import qualified Data.Text as T
import Language.Haskell.DoNotation
import Types.AppState
import Types.Models.RequestDef
import Types.Models.Response (ResponseIndex (..))
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

deleteResponse ::
  IxMonadState m => RequestDefContext -> ResponseIndex -> m (AppState a) (AppState a) ()
deleteResponse (RequestDefContext _ rid) (ResponseIndex i) = do
  s <- iget
  imodify $ responses . at rid . _Just . ix (currentEnvironmentKey s) %~ S.deleteAt i

deleteResponseWarning :: T.Text
deleteResponseWarning = "Are you sure you want to delete this response?"
