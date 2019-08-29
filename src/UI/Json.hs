{-# LANGUAGE OverloadedStrings #-}

module UI.Json
  ( readOnlyJson
  )
where

import           Brick                          ( Widget
                                                , emptyWidget
                                                , txt
                                                , txtWrap
                                                , vBox
                                                , withAttr
                                                , (<+>)
                                                )

import           Data.Either.Combinators        ( rightToMaybe )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Parsing.JsonParser             ( EndingComma(..)
                                                , Indentation(..)
                                                , JsonKey(..)
                                                , JsonLine(..)
                                                , JsonValue(..)
                                                , parseLine
                                                )
import           Text.Megaparsec                ( runParser )
import           Types.Brick.Name               ( Name )
import           UI.Attr
import           Utils.Text                     ( tryPretty )

indent :: Indentation -> Widget Name
indent (Indentation i) = txt i

renderKey :: JsonKey -> Widget Name
renderKey (JsonKey key) = withAttr jsonKeyAttr $ txtWrap $ "\"" <> key <> "\""

renderValue :: JsonValue -> Widget Name
renderValue v = case v of
  JsonStringValue t     -> withAttr jsonStringAttr $ txtWrap $ "\"" <> t <> "\""
  JsonNumber      t     -> withAttr jsonNumberAttr $ txtWrap t
  JsonBool        True  -> withAttr jsonBoolAttr $ txtWrap "true"
  JsonBool        False -> withAttr jsonBoolAttr $ txtWrap "false"
  JsonNull              -> withAttr jsonNullAttr $ txtWrap "null"

renderComma :: EndingComma -> Widget Name
renderComma (EndingComma True ) = txt ","
renderComma (EndingComma False) = emptyWidget

colorizedJsonLine :: JsonLine -> Widget Name
colorizedJsonLine (JustLCB i        ) = indent i <+> txt "{"
colorizedJsonLine (JustLSB i        ) = indent i <+> txt "["
colorizedJsonLine (ObjectStart i key) = indent i <+> renderKey key <+> txt ": {"
colorizedJsonLine (KeyValue i key value comma) =
  indent i <+> renderKey key <+> txt ": " <+> renderValue value <+> renderComma comma
colorizedJsonLine (EmptyArray i key comma) =
  indent i <+> renderKey key <+> txt ": []" <+> renderComma comma
colorizedJsonLine (EmptyObject i key comma) =
  indent i <+> renderKey key <+> txt ": {}" <+> renderComma comma
colorizedJsonLine (ArrayStart i key) = indent i <+> renderKey key <+> txt ": ["
colorizedJsonLine (ValueInArray i value comma) =
  indent i <+> renderValue value <+> renderComma comma
colorizedJsonLine (JustRCB i comma) = indent i <+> txt "}" <+> renderComma comma
colorizedJsonLine (JustRSB i comma) = indent i <+> txt "]" <+> renderComma comma

-- Tries to 1. parse the text as JSON, 2. pretty-print it, 3. re-parse it with our custom
-- parser made for colorizing, 4. assign attributes based on the results of (3).
-- If (1) or (2) fails, just display the plain text. If (3) fails, display the non-colorized
-- but pretty-printed JSON.
readOnlyJson :: T.Text -> Widget Name
readOnlyJson t = case tryPretty t of
  Nothing   -> txtWrap t
  Just json -> fromMaybe (txtWrap json) $ do
    parsed <- (rightToMaybe . sequence) $ runParser parseLine "JSON response body" <$> T.lines json
    return $ vBox (colorizedJsonLine <$> parsed)
