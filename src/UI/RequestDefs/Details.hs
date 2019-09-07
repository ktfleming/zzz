{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.RequestDefs.Details where

import           Brick                          ( Padding(Pad)
                                                , Widget
                                                , padLeft
                                                , txt
                                                , vBox
                                                , (<+>)
                                                )
import           Brick.Focus                    ( focusRing )
import           Brick.Widgets.List             ( list )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Fields
import           Types.Models.KeyValue          ( KeyValue
                                                , isEnabled
                                                , keyValueIso
                                                )
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Models.Screen
import           UI.Attr
import           UI.Forms.KeyValueList          ( readOnlyKeyValues )
import           UI.List                        ( ZZZList )
import           UI.Text                        ( explanationWithAttr
                                                , methodWidget
                                                )
import           UI.Url                         ( colorizedUrl )

makeResponseList :: Seq Response -> ZZZList Response
makeResponseList rs = list ResponseList rs 1

showRequestDefDetails
  :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState 'RequestDefDetailsTag) ()
showRequestDefDetails c = do
  s <- iget
  let ring = focusRing [RequestDetails, ResponseList, ResponseBodyDetails]
  imodify $ screen .~ RequestDefDetailsScreen c (makeResponseList (lookupResponses s c)) ring

refreshResponseList
  :: Monad m => IxStateT m (AppState 'RequestDefDetailsTag) (AppState 'RequestDefDetailsTag) ()
refreshResponseList = do
  s <- iget
  let RequestDefDetailsScreen c _ ring = s ^. screen
  imodify $ screen .~ RequestDefDetailsScreen c (makeResponseList (lookupResponses s c)) ring


requestDefDetailsWidget
  :: AppState 'RequestDefDetailsTag -> RequestDefContext -> Bool -> Widget Name
requestDefDetailsWidget s c@(RequestDefContext _ rid) focused =
  let r                = lookupRequestDef s c
      hasActiveRequest = Map.member rid (s ^. activeRequests)
      titleWidget      = txt "Request: " <+> methodWidget (r ^. method) <+> padLeft
        (Pad 1)
        (colorizedUrl (currentVariables s) (r ^. url))
      keyValues :: Seq KeyValue = (^. keyValueIso) <$> S.filter isEnabled (r ^. headers)
      headersWidget             = txt "Headers: " <+> readOnlyKeyValues keyValues
      explanation               = case (hasActiveRequest, focused) of
        (True, _) ->
          [ explanationWithAttr importantExplanationAttr
                                "Currently sending request -- press x to cancel"
          ]
        (False, True ) -> [explanationWithAttr explanationAttr "Press ENTER to send this request"]
        (False, False) -> []
  in  vBox $ explanation <> [titleWidget, headersWidget]
