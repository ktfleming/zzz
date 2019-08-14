{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI.RequestDefinitions.Details where

import           Brick                          ( Widget
                                                , txt
                                                )
import           Brick.Focus                    ( focusRing )
import           Brick.Widgets.List             ( list )
import           Control.Lens
import           Data.Generics.Product.Typed    ( typed )
import           Data.Sequence                  ( Seq )
import           Types.AppState
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Displayable      ( display )
import           Types.Models.RequestDefinition
import           Types.Models.Response
import           Types.Models.Screen
import           Types.Models.Url               ( Url(..) )
import           UI.List                        ( ZZZList )

makeResponseList :: Seq Response -> ZZZList Response
makeResponseList rs = list ResponseList rs 1

updateResponseList :: AppState -> ZZZList Response -> AppState
updateResponseList s l =
  s & screen . _RequestDetailsScreen . typed @(ZZZList Response) .~ l

showRequestDefinitionDetails :: AppState -> RequestDefinitionContext -> AppState
showRequestDefinitionDetails s c =
  let rs   = lookupResponses s c
      ring = focusRing [ResponseList, ResponseBody]
  in  s & screen .~ RequestDetailsScreen c (makeResponseList rs) ring

requestDefinitionDetailsWidget
  :: AppState -> RequestDefinitionContext -> Widget Name
requestDefinitionDetailsWidget s c =
  let r = lookupRequestDefinition s c
      fullText =
          "Request: " <> display (r ^. method) <> " " <> (r ^. url . coerced)
  in  txt fullText
