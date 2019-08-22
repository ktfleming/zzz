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
