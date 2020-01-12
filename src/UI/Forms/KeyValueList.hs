{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Forms.KeyValueList
  ( makeKeyValueForm,
    readOnlyKeyValues,
  )
where

import Brick
import Brick.Forms (FormFieldState (..))
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Control.Lens
import Data.Foldable (toList)
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Text as T
import Data.Text (Text)
import Types.Brick.CustomEvent (CustomEvent)
import Types.Brick.Name (Name (..))
import Types.Classes.Fields
import Types.Models.KeyValue
import UI.Attr
  ( disabledAttr,
    keyValueKeyAttr,
    keyValueValueAttr,
  )
import UI.Form (renderText)
import UI.Forms.FocusAwareEditor (focusAwareEditField)

-- Splits a Text into everything before the first '=', and everything after it
-- Also returns whether or not a '=' was actually present in the original text,
-- in order to distinguish between texts like 'a=' and 'a'.
parseKeyValue :: Text -> (Text, Text, Bool)
parseKeyValue t =
  let (left, right) = T.breakOn "=" t
      -- `left` is done, but now right is either empty, or it starts with a '=' character.
      -- Have to strip the first '=' in the latter case
      (fixedRight, hasEquals) = if T.null right then (right, False) else (T.drop 1 right, True)
   in (left, fixedRight, hasEquals)

makeKeyValueForm ::
  forall a b.
  KeyValueIso b =>
  Lens' a (Seq b) ->
  Name ->
  a ->
  FormFieldState a CustomEvent Name
makeKeyValueForm modelLens n s =
  let initFn :: Seq KeyValue -> Text
      initFn =
        let oneKeyValueText :: KeyValue -> Text
            oneKeyValueText (KeyValue k v) = k <> "=" <> v
         in T.intercalate "\n" . toList . fmap oneKeyValueText
      -- Validation: each line must contain at least one '=' character;
      -- this will be used to separate the line into the key and value
      -- (at the first such character, if more than one are present)
      validate :: [Text] -> Maybe (Seq KeyValue)
      validate [] = Just Seq.empty
      validate ts =
        let textToKeyValue :: Text -> Maybe KeyValue
            textToKeyValue t =
              let (left, right, _) = parseKeyValue t
               in if T.null right
                    then Nothing -- either no '=', or '=' is the last character
                    else Just $ KeyValue left right
         in (sequence . Seq.fromList . fmap textToKeyValue) (Prelude.filter (not . T.null) ts)
      readOnlyRender :: [Text] -> Widget name
      readOnlyRender = vLimit 10 . vBox . fmap readOnlyRenderOneLine
      readOnlyRenderOneLine :: Text -> Widget name
      readOnlyRenderOneLine t
        | (not . isTextEnabled) t = withAttr disabledAttr $ txt (t <> " (disabled)")
        | hasEquals = leftWidget <+> equalsWidget <+> rightWidget
        | otherwise = leftWidget
        where
          (left, right, hasEquals) = parseKeyValue t
          leftWidget = withAttr keyValueKeyAttr $ txt left
          equalsWidget = txt "="
          rightWidget = withAttr keyValueValueAttr $ txt right
      augment :: Widget Name -> Widget Name
      augment = vLimit 10 . withBorderStyle unicodeRounded . border
      -- Have to use the provided `modelLens` (which is a lens from the form state to
      -- the specific type that's represented as a KeyValue -- header, environment variable,
      -- etc) to make a more general Lens that works with KeyValues, in order to use it
      -- with this form that only deals with KeyValues. This is why we need the
      -- KeyValueIso b constraint.
      -- (There may be a more idiomatic way to do this with simpler lens code, but this
      -- works for now.)
      keyValueLens :: Lens' a (Seq KeyValue)
      keyValueLens = lens getter setter
        where
          getter x = view keyValueIso <$> x ^. modelLens
          setter x kvs = let bs = view (from keyValueIso) <$> kvs in x & modelLens .~ bs
   in focusAwareEditField
        keyValueLens
        n
        Nothing
        initFn
        validate
        renderText
        augment
        (Just readOnlyRender)
        s

-- Similar to the read-only rendering in the form, but works directly on a valid
-- KeyValue record, rather than plain Text that might not be valid.
readOnlyKeyValue :: KeyValue -> Widget Name
readOnlyKeyValue kv =
  let nameWidget = withAttr keyValueKeyAttr $ txt $ kv ^. name
      equalsWidget = txt "="
      valueWidget = withAttr keyValueValueAttr $ txt $ kv ^. value
   in nameWidget <+> equalsWidget <+> valueWidget

readOnlyKeyValues :: Seq KeyValue -> Widget Name
readOnlyKeyValues = vBox . fmap readOnlyKeyValue . toList
