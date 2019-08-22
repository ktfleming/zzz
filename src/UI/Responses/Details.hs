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
                                                , txt
                                                , txtWrap
                                                , vBox
                                                , viewport
                                                , withAttr
                                                , (<+>)
                                                , (<=>)
                                                )
import           Brick.Widgets.Center           ( hCenter )
import           Brick.Widgets.List             ( listSelectedFocusedAttr )
import           Control.Lens
import           Data.Aeson                     ( Value
                                                , decode
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.Either.Combinators        ( rightToMaybe )
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence                  ( Seq )
import           Data.String.Conversions        ( cs )
import           Data.Text                     as T
import           Parsing.JsonParser             ( parseLine )
import           Text.Megaparsec                ( runParser )
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Fields
import           Types.Models.Header            ( Header )
import           Types.Models.Response
import           Types.Models.Url
import           UI.Forms.Headers               ( readOnlyHeaders )
import           UI.Json                        ( colorizedJsonLine )

-- Tries to 1. parse the text as JSON, 2. pretty-print it, 3. re-parse it with our custom
-- parser made for colorizing, 4. assign attributes based on the results of (3).
-- If any of these fail, just fall back to displaying the plain text.
responseBodyWidget :: T.Text -> Widget Name
responseBodyWidget t = fromMaybe (txtWrap t) $ do
  decoded <- (decode . cs) t :: Maybe Value
  let prettied = (cs . encodePretty) decoded
  parsed <-
    (rightToMaybe . sequence) $ runParser parseLine "JSON response body" <$> T.lines prettied
  return $ vBox (colorizedJsonLine <$> parsed)

-- Response body plus URL, request body, and headers
responseBodyViewport :: Response -> Widget Name
responseBodyViewport r =
  let b :: T.Text      = r ^. body . coerced
      u :: T.Text      = r ^. url . coerced
      hs :: Seq Header = r ^. headers
      urlWidget        = txt $ "URL:     " <> u
      headersWidget    = txt "Headers: " <+> readOnlyHeaders hs
  in  viewport ResponseBodyViewport Vertical
        $   urlWidget
        <=> padBottom (Pad 1) headersWidget
        <=> txt "Response body:"
        <=> responseBodyWidget b

-- The body text plus an optional message at the top
responseDetails :: Response -> Bool -> Widget Name
responseDetails r focused = if focused
  then
    let
      explanation =
        padBottom (Pad 1)
          $ withAttr listSelectedFocusedAttr
          $ hCenter
          $ txtWrap
              "Response body focused -- use the arrow keys to scroll, or TAB to return to the response list."
    in  explanation <=> responseBodyViewport r
  else responseBodyViewport r
