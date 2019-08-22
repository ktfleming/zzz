{-# LANGUAGE OverloadedStrings #-}

module UI.Json
  ( colorizedJsonLine
  )
where

import           Brick                          ( Widget
                                                , emptyWidget
                                                , txt
                                                , txtWrap
                                                , withAttr
                                                , (<+>)
                                                )

import           Parsing.JsonParser             ( EndingComma(..)
                                                , Indentation(..)
                                                , JsonKey(..)
                                                , JsonLine(..)
                                                , JsonValue(..)
                                                )
import           Types.Brick.Name               ( Name )
import           UI.Attr

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
colorizedJsonLine (ArrayStart i key  ) = indent i <+> renderKey key <+> txt ": ["
colorizedJsonLine (JustRCB    i comma) = indent i <+> txt "}" <+> renderComma comma
colorizedJsonLine (JustRSB    i comma) = indent i <+> txt "]" <+> renderComma comma
