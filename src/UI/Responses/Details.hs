{-# LANGUAGE OverloadedStrings #-}

module UI.Responses.Details
  ( responseBodyWidget
  )
where

import           Brick                          ( Padding(..)
                                                , ViewportType(Vertical)
                                                , Widget
                                                , padBottom
                                                , txtWrap
                                                , viewport
                                                , withAttr
                                                , (<=>)
                                                )
import           Brick.Widgets.Center           ( hCenter )
import           Brick.Widgets.List             ( listSelectedFocusedAttr )
import           Control.Lens
import           Data.Aeson                     ( Value
                                                , decode
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.String.Conversions        ( cs )
import           Types.Brick.Name               ( Name(..) )
import           Types.Models.Response

-- Just the body text itself
responseBodyViewport :: Response -> Widget Name
responseBodyViewport r =
  let b          = r ^. body
      decoded    = (decode . cs) b :: Maybe Value
      baseWidget = case decoded of
        Nothing -> txtWrap b
        Just v  -> (txtWrap . cs) $ encodePretty v
  in  viewport ResponseBodyViewport Vertical baseWidget

-- The body text plus an optional message at the top
responseBodyWidget :: Response -> Bool -> Widget Name
responseBodyWidget r focused = if focused
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
