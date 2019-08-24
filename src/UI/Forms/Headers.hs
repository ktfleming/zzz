{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Forms.Headers
  ( makeHeadersForm
  , readOnlyHeaders
  )
where

import           Brick                          ( Widget
                                                , txt
                                                , vBox
                                                , vLimit
                                                , withAttr
                                                , withBorderStyle
                                                , (<+>)
                                                )
import           Brick.Forms                    ( FormFieldState(..) )
import           Brick.Widgets.Border           ( border )
import           Brick.Widgets.Border.Style     ( unicodeRounded )
import           Control.Lens
import           Data.Foldable                  ( toList )
import           Data.Sequence                 as S
import           Data.Sequence                  ( Seq )
import           Data.Text                     as T
import           Types.Brick.CustomEvent        ( CustomEvent )
import           Types.Brick.Name               ( Name(..) )
import           Types.Classes.Fields
import           Types.Models.Header
import           Types.Models.RequestDef
import           UI.Attr                        ( disabledAttr
                                                , headerNameAttr
                                                , headerValueAttr
                                                )
import           UI.Form                        ( renderText )
import           UI.Forms.FocusAwareEditor      ( focusAwareEditField )

-- Splits a Text into everything before the first '=', and everything after it
-- Also returns whether or not a '=' was actually present in the original text,
-- in order to distinguish between texts like 'a=' and 'a'.
parseHeader :: T.Text -> (T.Text, T.Text, Bool)
parseHeader t =
  let (left      , right    ) = T.breakOn "=" t

      -- `left` is done, but now right is either empty, or it starts with a '=' character.
      -- Have to strip the first '=' in the latter case
      (fixedRight, hasEquals) = if T.null right then (right, False) else (T.drop 1 right, True)
  in  (left, fixedRight, hasEquals)

makeHeadersForm :: RequestDefFormState -> FormFieldState RequestDefFormState CustomEvent Name
makeHeadersForm s =
  let initFn :: Seq Header -> T.Text
      initFn =
          let oneHeaderText :: Header -> T.Text
              oneHeaderText h = (h ^. name . coerced) <> "=" <> (h ^. value . coerced)
          in  T.intercalate "\n" . toList . fmap oneHeaderText

      -- Validation: each line must contain at least one '=' character;
      -- this will be used to separate the line into the header name and value
      -- (at the first such character, if more than one are present)
      validate :: [T.Text] -> Maybe (Seq Header)
      validate [] = Just S.empty
      validate ts =
          let textToHeader :: Text -> Maybe Header
              textToHeader t =
                  let (left, right, _) = parseHeader t
                  in  if T.null right
                        then Nothing -- either no '=', or '=' is the last character
                        else Just Header { headerName  = HeaderName left
                                         , headerValue = HeaderValue right
                                         }
          in  (sequence . S.fromList . fmap textToHeader) (Prelude.filter (not . T.null) ts)

      readOnlyRender :: [T.Text] -> Widget name
      readOnlyRender = vBox . fmap readOnlyRenderOneLine

      readOnlyRenderOneLine :: T.Text -> Widget name
      readOnlyRenderOneLine t
        | (not . isHeaderTextEnabled) t = withAttr disabledAttr $ txt (t <> " (disabled)")
        | hasEquals                     = leftWidget <+> equalsWidget <+> rightWidget
        | otherwise                     = leftWidget
         where
          (left, right, hasEquals) = parseHeader t
          leftWidget               = withAttr headerNameAttr $ txt left
          equalsWidget             = txt "="
          rightWidget              = withAttr headerValueAttr $ txt right

      augment :: Widget Name -> Widget Name
      augment = vLimit 10 . withBorderStyle unicodeRounded . border
  in  focusAwareEditField headers
                          HeadersField
                          Nothing
                          initFn
                          validate
                          renderText
                          augment
                          (Just readOnlyRender)
                          s

-- Similar to the read-only rendering in the form, but works directly on a valid
-- Header record, rather than plain Text that might not be valid. Used on the
-- RequestDef details page.
readOnlyHeader :: Header -> Widget Name
readOnlyHeader h =
  let nameWidget   = withAttr headerNameAttr $ txt $ h ^. name . coerced
      equalsWidget = txt "="
      valueWidget  = withAttr headerValueAttr $ txt $ h ^. value . coerced
  in  nameWidget <+> equalsWidget <+> valueWidget

readOnlyHeaders :: Seq Header -> Widget Name
readOnlyHeaders = vBox . fmap readOnlyHeader . toList
