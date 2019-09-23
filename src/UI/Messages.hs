{-# LANGUAGE OverloadedStrings #-}

module UI.Messages
  ( messageWidget
  )
where

import           Brick                          ( ViewportType(Vertical)
                                                , Widget
                                                , str
                                                , txt
                                                , vBox
                                                , viewport
                                                , withAttr
                                                , (<+>)
                                                )
import           Control.Lens
import           Data.Foldable                  ( toList )
import           Data.Sequence                  ( Seq )
import           Data.Time.ISO8601              ( formatISO8601 )
import           Types.AppState                 ( Message(..) )
import           Types.Brick.Name               ( Name(MessagesViewport) )
import           Types.Classes.Fields
import           UI.Attr                        ( timestampAttr )

oneMessage :: Message -> Widget Name
oneMessage m =
  (withAttr timestampAttr . str . formatISO8601) (m ^. dateTime) <+> txt ": " <+> txt (m ^. text)

messageWidget :: Seq Message -> Widget Name
messageWidget ms = viewport MessagesViewport Vertical $ vBox $ oneMessage <$> toList ms
