{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module UI.RequestDefs.Details where

import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )

import           Brick                          ( EventM
                                                , Widget
                                                , txt
                                                )
import           Brick.Focus                    ( focusRing )
import           Brick.Widgets.List             ( handleListEvent
                                                , list
                                                )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import           Control.Monad.Indexed.Trans    ( ilift )
import           Data.Sequence                  ( Seq )
import           Data.String                    ( fromString )
import           Graphics.Vty                   ( Event(EvKey)
                                                , Key
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Displayable      ( display )
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.List                        ( ZZZList )

makeResponseList :: Seq Response -> ZZZList Response
makeResponseList rs = list ResponseList rs 1

updateResponseList
  :: Key
  -> IxStateT
       (EventM Name)
       (AppState 'RequestDefDetailsTag)
       (AppState 'RequestDefDetailsTag)
       ()
updateResponseList key = do
  s <- iget
  let RequestDefDetailsScreen c l ring = s ^. screen
  updatedList <- ilift $ handleListEvent (EvKey key []) l
  imodify $ screen .~ RequestDefDetailsScreen c updatedList ring

showRequestDefDetails
  :: Monad m
  => RequestDefContext
  -> IxStateT m (AppState a) (AppState 'RequestDefDetailsTag) ()
showRequestDefDetails c = do
  s <- iget
  let rs   = lookupResponses s c
      ring = focusRing [ResponseList, ResponseBody]
  imodify $ screen .~ RequestDefDetailsScreen c (makeResponseList rs) ring

requestDefDetailsWidget :: AppState a -> RequestDefContext -> Widget Name
requestDefDetailsWidget s c =
  let r = lookupRequestDef s c
      fullText =
          "Request: " <> display (r ^. method) <> " " <> (r ^. url . coerced)
  in  txt fullText
