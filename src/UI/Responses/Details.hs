{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Responses.Details
  ( responseDetails
  )
where

import           Brick                          ( Padding(..)
                                                , ViewportType(Vertical)
                                                , Widget
                                                , padBottom
                                                , str
                                                , txt
                                                , vBox
                                                , viewport
                                                , (<+>)
                                                , (<=>)
                                                )
import           Brick.Widgets.Center           ( hCenterWith )
import           Control.Lens
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import           Data.Text                     as T
import           Data.Time.ISO8601              ( formatISO8601 )
import           Numeric                        ( showFFloat )
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Fields
import           Types.Models.Header            ( Header )
import           Types.Models.RequestDef        ( RequestBody(..) )
import           Types.Models.Response
import           Types.Models.Url
import           UI.Attr
import           UI.Forms.Headers               ( readOnlyHeaders )
import           UI.Json                        ( readOnlyJson )
import           UI.Text                        ( explanationWithAttr
                                                , methodWidget
                                                )

centerSection :: T.Text -> Widget Name
centerSection t = hCenterWith (Just '-') (txt (" " <> t <> " "))

-- Response body plus URL, request body, and headers
responseBodyViewport :: Response -> Widget Name
responseBodyViewport r =
  let u :: T.Text             = r ^. url . coerced
      sentBody :: T.Text      = r ^. requestBody . coerced
      hs :: Seq Header        = r ^. headers
      elapsedMillis :: Double = realToFrac (r ^. elapsedTime) * 1000
      elapsedWidget           = str $ showFFloat (Just 0) elapsedMillis " ms"
  in  viewport ResponseBodyViewport Vertical $ vBox $ fst <$> Prelude.filter
        snd
        [ (txt "Request:  " <+> methodWidget (r ^. method) <+> txt (" " <> u), True)
        , (str $ "Received: " <> formatISO8601 (r ^. dateTime), True)
        , (padBottom (Pad 1) $ txt "Elapsed:  " <+> elapsedWidget, True)
        , (centerSection "Headers"           , not (S.null hs))
        , (readOnlyHeaders hs                , not (S.null hs))
        , (centerSection "Request Body"      , not (T.null sentBody))
        , (readOnlyJson sentBody             , not (T.null sentBody))
        , (centerSection "Response Body"     , True)
        , (readOnlyJson (r ^. body . coerced), True)
        , (centerSection "End"               , True)
        ]

-- The body text plus an optional message at the top
responseDetails :: Response -> Bool -> Widget Name
responseDetails r focused = if focused
  then
    let explanation = explanationWithAttr
          explanationAttr
          "Response body focused -- use the arrow keys to scroll, or TAB to switch focus"
    in  explanation <=> responseBodyViewport r
  else responseBodyViewport r
