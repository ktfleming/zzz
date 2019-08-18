{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.RequestDefs.Details where

import           Brick                          ( EventM
                                                , Widget
                                                , txt
                                                )
import           Brick.Focus                    ( focusRing )
import           Brick.Widgets.List             ( handleListEvent
                                                , list
                                                )
import           Control.Lens
import           Data.Generics.Product.Typed    ( typed )
import           Data.Sequence                  ( Seq )
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

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State      ( StateT
                                                , get
                                                , modify
                                                )

makeResponseList :: Seq Response -> ZZZList Response
makeResponseList rs = list ResponseList rs 1

updateResponseList
  :: ZZZList Response -> Key -> StateT AppState (EventM Name) ()
updateResponseList l key = do
  updatedList <- lift $ handleListEvent (EvKey key []) l
  modify
    $  screen
    .  _RequestDefDetailsScreen
    .  typed @(ZZZList Response)
    .~ updatedList

showRequestDefDetails :: Monad m => RequestDefContext -> StateT AppState m ()
showRequestDefDetails c = do
  s <- get
  let rs   = lookupResponses s c
      ring = focusRing [ResponseList, ResponseBody]
  modify $ screen .~ RequestDefDetailsScreen c (makeResponseList rs) ring

requestDefDetailsWidget :: AppState -> RequestDefContext -> Widget Name
requestDefDetailsWidget s c =
  let r = lookupRequestDef s c
      fullText =
          "Request: " <> display (r ^. method) <> " " <> (r ^. url . coerced)
  in  txt fullText
