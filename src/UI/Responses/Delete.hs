{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.Responses.Delete
  ( deleteResponse
  , deleteResponseWarning
  )
where

import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Models.RequestDef
import           Types.Models.Response          ( ResponseIndex(..) )

deleteResponse
  :: Monad m => RequestDefContext -> ResponseIndex -> IxStateT m (AppState a) (AppState a) ()
deleteResponse (RequestDefContext _ rid) (ResponseIndex i) = do
  s <- iget
  imodify $ responses . at rid . _Just . ix (currentEnvironmentKey s) %~ S.deleteAt i

deleteResponseWarning :: T.Text
deleteResponseWarning = "Are you sure you want to delete this response?"
