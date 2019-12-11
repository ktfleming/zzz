{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Monad.Indexed ((>>>=))
import Control.Monad.Indexed.State
  ( IxMonadState,
    iget,
    imodify,
  )
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (fromString)
import qualified Data.Text as T
import Language.Haskell.DoNotation
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
import Types.Models.Screen.Optics (listLens)
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
import Utils.IfThenElse (ifThenElse)
import Prelude hiding
  ( Monad ((>>), (>>=), return),
    pure,
  )

makeResponseList :: Seq Response -> AppList Response
makeResponseList rs = AppList $ list ResponseList rs 1

showRequestDefDetails ::
  IxMonadState m => RequestDefContext -> m (AppState a) (AppState 'RequestDefDetailsTag) ()
showRequestDefDetails c = iget >>>= \s ->
  let ring = AppFocusRing $ focusRing [RequestDetails, ResponseList, ResponseBodyDetails]
      ekey = currentEnvironmentKey s
   in imodify $
        screen
          .~ RequestDefDetailsScreen c (makeResponseList (lookupResponses s c ekey)) ring Nothing

refreshResponseList ::
  IxMonadState m => m (AppState 'RequestDefDetailsTag) (AppState 'RequestDefDetailsTag) ()
refreshResponseList = do
  s <- iget
  let RequestDefDetailsScreen c _ _ _ = s ^. screen -- TODO: lens (or just getter) for the context inside a screen?
      ekey = currentEnvironmentKey s
  imodify $ screen . listLens .~ makeResponseList (lookupResponses s c ekey)

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
          else txt "Body:    " <+> (readOnlyJson $ r ^. body . coerced)
      explanation = case (hasActiveRequest, focused) of
        (True, _) ->
          [ explanationWithAttr
              importantExplanationAttr
              "Currently sending request -- press x to cancel."
          ]
        _ -> []
      mainBox = vBox $ explanation <> [titleWidget, headersWidget, bodyWidget]
   in borderOrPad focused $ padLeft (Pad 2) $ mainBox

errorWidget :: Maybe RequestError -> Widget Name
errorWidget = maybe emptyWidget (padBottom (Pad 1) . withAttr errorAttr . hCenter . txtWrap . errorDescription)

responseHistoryWidget :: AppList ResponseWithCurrentTime -> Bool -> Widget Name
responseHistoryWidget appList@(AppList innerList) focused =
  let elems = listElements innerList
   in if null elems
        then emptyWidget
        else
          borderOrPad focused . vBox $
            [ (padLeft (Pad 2) $ txtWrap "Response history:"),
              (vLimit (min 10 (length elems)) (renderGenericList focused appList))
            ]

requestDefDetailsWidget :: AppState 'RequestDefDetailsTag -> Widget Name
requestDefDetailsWidget s =
  let (RequestDefDetailsScreen c (AppList zl) (AppFocusRing ring) maybeError) = s ^. screen
      focused = focusGetCurrent ring
      bodyWidget = case listSelectedElement zl of
        Just (_, r) -> borderOrPad (focused == Just ResponseBodyDetails) $ responseDetails r
        Nothing -> txtWrap "No response selected."
      listWithTime :: AppList ResponseWithCurrentTime
      listWithTime = coerce $ ResponseWithCurrentTime (s ^. currentTime) <$> zl
   in overrideAttr borderAttr focusedBorderAttr $ vBox $
        [ topWidget s c (focused == Just RequestDetails),
          errorWidget maybeError,
          responseHistoryWidget listWithTime (focused == Just ResponseList),
          bodyWidget
        ]
