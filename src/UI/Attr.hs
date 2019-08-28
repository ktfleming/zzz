module UI.Attr where

import           Brick.AttrMap                  ( AttrName
                                                , attrName
                                                )

headerNameAttr :: AttrName
headerNameAttr = attrName "headerName"

headerValueAttr :: AttrName
headerValueAttr = attrName "headerValue"

-- For disabled Headers, etc
disabledAttr :: AttrName
disabledAttr = attrName "disabled"

-- For colorized JSON

jsonKeyAttr :: AttrName
jsonKeyAttr = attrName "jsonKey"

-- This is for a value string, i.e. on the right side of the ':'
-- jsonKeyAttr is for the keys on the left side
jsonStringAttr :: AttrName
jsonStringAttr = attrName "jsonString"

jsonNumberAttr :: AttrName
jsonNumberAttr = attrName "jsonNumber"

jsonBoolAttr :: AttrName
jsonBoolAttr = attrName "jsonBool"

jsonNullAttr :: AttrName
jsonNullAttr = attrName "jsonNull"

-- HTTP methods
methodAttr :: AttrName
methodAttr = attrName "method"

-- Explanations

explanationAttr :: AttrName
explanationAttr = attrName "explanation"

importantExplanationAttr :: AttrName
importantExplanationAttr = explanationAttr <> attrName "important"
