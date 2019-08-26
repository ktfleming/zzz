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
                                                , viewport
                                                , withAttr
                                                , (<+>)
                                                , (<=>)
                                                )
import           Brick.Widgets.Center           ( hCenter )
import           Brick.Widgets.List             ( listSelectedFocusedAttr )
import           Control.Lens
import           Data.Sequence                  ( Seq )
import           Data.Text                     as T
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Fields
import           Types.Models.Header            ( Header )
import           Types.Models.RequestDef        ( RequestBody(..) )
import           Types.Models.Response
import           Types.Models.Url
import           UI.Forms.Headers               ( readOnlyHeaders )
import           UI.Json                        ( readOnlyJson )
import           UI.Text                        ( methodWidget )

-- Response body plus URL, request body, and headers
responseBodyViewport :: Response -> Widget Name
responseBodyViewport r =
  let u :: T.Text      = r ^. url . coerced
      hs :: Seq Header = r ^. headers
      requestWidget    = txt "Request: " <+> methodWidget (r ^. method) <+> txt (" " <> u)
      headersWidget    = txt "Headers: " <+> readOnlyHeaders hs
  in  viewport ResponseBodyViewport Vertical
        $   padBottom (Pad 1) requestWidget
        <=> padBottom (Pad 1) headersWidget
        <=> txt "Request body:"
        <=> padBottom (Pad 1) (readOnlyJson (r ^. requestBody . coerced))
        <=> txt "Response body:"
        <=> readOnlyJson (r ^. body . coerced)

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
