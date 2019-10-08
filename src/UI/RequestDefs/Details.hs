{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module UI.RequestDefs.Details
  ( requestDefDetailsWidget
  , showRequestDefDetails
  , refreshResponseList
  )
where

import           Brick                          ( Padding(Pad)
                                                , Widget
                                                , emptyWidget
                                                , padBottom
                                                , padLeft
                                                , txt
                                                , txtWrap
                                                , vBox
                                                , vLimit
                                                , withAttr
                                                , (<+>)
                                                )
import           Brick.Focus                    ( focusGetCurrent
                                                , focusRing
                                                )
import           Brick.Widgets.Border           ( hBorder )
import           Brick.Widgets.Center           ( hCenter )
import           Brick.Widgets.List             ( list
                                                , listElements
                                                , listSelectedElement
                                                )
import           Control.Lens
import           Control.Monad.Indexed.State    ( IxStateT
                                                , iget
                                                , imodify
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                     ( isJust )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Data.Time.ISO8601              ( formatISO8601 )
import           Language.Haskell.DoNotation
import           Prelude                 hiding ( Monad(..)
                                                , pure
                                                )
import           Types.AppState
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Displayable      ( display )
import           Types.Classes.Fields
import           Types.Classes.HasId            ( model )
import           Types.Models.KeyValue          ( KeyValue
                                                , isEnabled
                                                , keyValueIso
                                                )
import           Types.Models.RequestDef
import           Types.Models.Response
import           Types.Models.Screen
import           UI.Attr
import           UI.Events.BrickUpdates         ( listLens )
import           UI.Forms.KeyValueList          ( readOnlyKeyValues )
import           UI.List                        ( ZZZList
                                                , renderGenericList
                                                )
import           UI.Responses.Details           ( responseDetails )
import           UI.Text                        ( explanationWithAttr )
import           UI.Url                         ( colorizedUrl )

makeResponseList :: Seq Response -> ZZZList Response
makeResponseList rs = list ResponseList rs 1

showRequestDefDetails
  :: Monad m => RequestDefContext -> IxStateT m (AppState a) (AppState 'RequestDefDetailsTag) ()
showRequestDefDetails c = do
  s <- iget
  let ring = focusRing [RequestDetails, ResponseList, ResponseBodyDetails]
      ekey = currentEnvironmentKey s
  imodify $ screen .~ RequestDefDetailsScreen c (makeResponseList (lookupResponses s c ekey)) ring

refreshResponseList
  :: Monad m => IxStateT m (AppState 'RequestDefDetailsTag) (AppState 'RequestDefDetailsTag) ()
refreshResponseList = do
  s <- iget
  let RequestDefDetailsScreen c _ _ = s ^. screen
      ekey                          = currentEnvironmentKey s
  imodify $ screen . listLens .~ makeResponseList (lookupResponses s c ekey)

topWidget :: AppState 'RequestDefDetailsTag -> RequestDefContext -> Bool -> Widget Name
topWidget s c@(RequestDefContext _ rid) focused =
  let r                = model s c
      hasActiveRequest = Map.member rid (s ^. activeRequests)
      titleWidget      = txt "Request: " <+> display (r ^. method) <+> padLeft
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

errorDisplay :: LastError -> Widget Name
errorDisplay (LastError errorTime) =
  withAttr errorAttr
    $  hCenter
    $  txtWrap
    $  "The request sent at "
    <> (T.pack . formatISO8601) errorTime
    <> " failed. See the message log (CTRL+o) for more details."

requestDefDetailsWidget :: AppState 'RequestDefDetailsTag -> Widget Name
requestDefDetailsWidget s =
  let (RequestDefDetailsScreen c zzzList ring) = s ^. screen
      maybeError         = model s c ^. lastError
      focused            = focusGetCurrent ring
      requestFocused     = focused == Just RequestDetails
      historyListFocused = focused == Just ResponseList
      bodyFocused        = focused == Just ResponseBodyDetails
      hasResponses       = not $ null (listElements zzzList)

      bodyWidget         = case listSelectedElement zzzList of
        Just (_, r) -> responseDetails r bodyFocused
        Nothing     -> txtWrap "No response selected."

      listWithTime :: ZZZList ResponseWithCurrentTime
      listWithTime = ResponseWithCurrentTime (s^.currentTime) <$> zzzList

      allWidgets = fst <$> filter
        snd
        [ (padLeft (Pad 2) (topWidget s c requestFocused), True)
        , (hBorder, hasResponses || isJust maybeError)
        , maybe (emptyWidget, False) ((, True) . padBottom (Pad 1) . errorDisplay) maybeError
        , (padLeft (Pad 2) $ txtWrap "Response history:", hasResponses)
        , (vLimit 10 (renderGenericList historyListFocused listWithTime), hasResponses)
        , (hBorder   , not requestFocused)
        , (bodyWidget, not requestFocused)
        ]
  in  vBox allWidgets
