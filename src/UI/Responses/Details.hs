{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Responses.Details
  ( responseDetails,
  )
where

import Brick
import Brick.Widgets.Center (hCenterWith)
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import Numeric (showFFloat)
import Types.Brick.Name (Name (..))
import Types.Classes.Displayable (display)
import Types.Classes.Fields
import Types.Models.KeyValue
  ( KeyValue,
    keyValueIso,
  )
import Types.Models.RequestDef (RequestBody (..))
import Types.Models.Response
import Types.Models.Url
import UI.Forms.KeyValueList (readOnlyKeyValues)
import UI.Json (readOnlyJson)

centerSection :: Text -> Widget Name
centerSection t = hCenterWith (Just '-') (txt (" " <> t <> " "))

-- Response body plus URL, request body, and headers
responseDetails :: TimeZone -> Response -> Widget Name
responseDetails tz r =
  let u :: Text = r ^. url . coerced
      sentBody :: Text = r ^. requestBody . coerced
      keyValues :: Seq KeyValue = fmap (view keyValueIso) (r ^. headers)
      elapsedMillis :: Double = realToFrac (r ^. elapsedTime) * 1000
      elapsedWidget = str $ showFFloat (Just 0) elapsedMillis " ms"
   in viewport ResponseBodyViewport Vertical $ vBox $
        fst
          <$> Prelude.filter
            snd
            [ (txt "Request:  " <+> display (r ^. method) <+> txt (" " <> u), True),
              (str $ "Received: " <> iso8601Show (utcToZonedTime tz (r ^. dateTime)), True),
              (txt "Elapsed:  " <+> elapsedWidget, True),
              (padBottom (Pad 1) $ txt "Status:   " <+> display (r ^. statusCode), True),
              (centerSection "Headers", not (Seq.null keyValues)),
              (readOnlyKeyValues keyValues, not (Seq.null keyValues)),
              (centerSection "Request Body", not (T.null sentBody)),
              (readOnlyJson sentBody, not (T.null sentBody)),
              (centerSection "Response Body", True),
              (readOnlyJson (r ^. body . coerced), True),
              (centerSection "End", True)
            ]
