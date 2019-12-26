{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.RequestDefs.Details
  ( requestDefDetailsWidget,
    showRequestDefDetails,
    refreshResponseList,
    makeResponseList,
  )
where

import Brick
import Brick.Focus
  ( focusGetCurrent,
    focusRing,
  )
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List
  ( list,
    listElements,
    listSelectedElement,
  )
import Control.Lens
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Types.AppState
import Types.Brick.Name (Name (..))
import Types.Classes.Displayable (display)
import Types.Classes.Fields
import Types.Classes.HasId (model)
import Types.Models.KeyValue
  ( KeyValue,
    isEnabled,
    keyValueIso,
  )
import Types.Models.RequestDef
import Types.Models.Response
import Types.Models.Screen
import Types.Models.Screen.Optics (context, listLens)
import UI.Attr
import UI.FocusRing (AppFocusRing (..))
import UI.Forms.KeyValueList (readOnlyKeyValues)
import UI.Json (readOnlyJson)
import UI.List
  ( AppList (..),
    renderGenericList,
  )
import UI.Responses.Details (responseDetails)
import UI.Text (explanationWithAttr)
import UI.Url (colorizedUrl)

makeResponseList :: Seq Response -> AppList Response
makeResponseList rs = AppList $ list ResponseList rs 1

showRequestDefDetails ::
  RequestDefContext -> AppState a -> AppState 'RequestDefDetailsTag
showRequestDefDetails c s =
  let ring = AppFocusRing $ focusRing [RequestDetails, ResponseList, ResponseBodyDetails]
      ekey = currentEnvironmentKey s
   in s & screen .~ RequestDefDetailsScreen c (makeResponseList (lookupResponses s c ekey)) ring Nothing

refreshResponseList ::
  AppState 'RequestDefDetailsTag -> AppState 'RequestDefDetailsTag
refreshResponseList s =
  let c = s ^. screen ^. context
      ekey = currentEnvironmentKey s
   in s & screen . listLens .~ makeResponseList (lookupResponses s c ekey)

-- Surround the provided widget with a border if it's focused, or just
-- pad every side by 1 if not (to ensure it's the same size whether focused or not)
borderOrPad :: Bool -> Widget Name -> Widget Name
borderOrPad focused = if focused then border else padAll 1

-- Displays the basic RequestDef info (URL, method, etc) as well as a warning
-- if a variable used somewhere is not defined in the current environment
topWidget :: AppState 'RequestDefDetailsTag -> RequestDefContext -> Bool -> Widget Name
topWidget s c@(RequestDefContext _ rid) focused =
  let r = model s c
      hasActiveRequest = Map.member rid (s ^. activeRequests)
      titleWidget =
        txt "Request: " <+> display (r ^. method)
          <+> padLeft
            (Pad 1)
            (colorizedUrl (currentVariables s) (r ^. url))
      keyValues :: Seq KeyValue = (^. keyValueIso) <$> Seq.filter isEnabled (r ^. headers)
      headersWidget =
        if Seq.null keyValues
          then emptyWidget
          else txt "Headers: " <+> readOnlyKeyValues keyValues
      bodyWidget =
        if T.null (r ^. body . coerced)
          then emptyWidget
          else txt "Body:    " <+> readOnlyJson (r ^. body . coerced)
      explanation = case (hasActiveRequest, focused) of
        (True, _) ->
          [ explanationWithAttr
              importantExplanationAttr
              "Currently sending request -- press x to cancel."
          ]
        _ -> []
      mainBox = vBox $ explanation <> [titleWidget, headersWidget, bodyWidget]
   in borderOrPad focused $ padLeft (Pad 2) mainBox

errorWidget :: Maybe RequestError -> Widget Name
errorWidget = maybe emptyWidget (padBottom (Pad 1) . withAttr errorAttr . hCenter . txtWrap . errorDescription)

responseHistoryWidget :: AppList ResponseWithCurrentTime -> Bool -> Bool -> Widget Name
responseHistoryWidget appList@(AppList innerList) focused showSelection =
  let elems = listElements innerList
   in if null elems
        then emptyWidget
        else
          borderOrPad focused . vBox $
            [ padLeft (Pad 2) $ txtWrap "Response history:",
              vLimit (min 10 (length elems)) (renderGenericList focused showSelection appList)
            ]

requestDefDetailsWidget :: AppState 'RequestDefDetailsTag -> Widget Name
requestDefDetailsWidget s =
  let (RequestDefDetailsScreen c (AppList zl) (AppFocusRing ring) maybeError) = s ^. screen
      focused = focusGetCurrent ring
      showResponse = focused == Just ResponseBodyDetails || focused == Just ResponseList
      bodyWidget = case (showResponse, listSelectedElement zl) of
        (True, Just (_, r)) -> borderOrPad (focused == Just ResponseBodyDetails) $ responseDetails r
        _ -> emptyWidget
      listWithTime :: AppList ResponseWithCurrentTime
      listWithTime = coerce $ ResponseWithCurrentTime (s ^. currentTime) <$> zl
   in overrideAttr borderAttr focusedBorderAttr $
        vBox
          [ topWidget s c (focused == Just RequestDetails),
            errorWidget maybeError,
            responseHistoryWidget listWithTime (focused == Just ResponseList) (focused /= Just RequestDetails),
            bodyWidget
          ]
