module UI.Attr where

import           Brick.AttrMap                  ( AttrName
                                                , attrName
                                                )

errorAttr :: AttrName
errorAttr = attrName "error"

keyValueKeyAttr :: AttrName
keyValueKeyAttr = attrName "keyValueKey"

keyValueValueAttr :: AttrName
keyValueValueAttr = attrName "keyValueValue"

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

-- Search screen
searchPlaceholderAttr :: AttrName
searchPlaceholderAttr = attrName "searchPlaceholder"

-- For variables that get substituted into a template
templatedVariableAttr :: AttrName
templatedVariableAttr = attrName "templatedVariable"

statusBarAttr :: AttrName
statusBarAttr = attrName "statusBar"
