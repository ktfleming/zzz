{-# LANGUAGE OverloadedStrings #-}

module UI.List where

import           Brick                          ( Widget
                                                , padLeft
                                                , padRight
                                                , txt
                                                , (<+>)
                                                )
import           Brick.Types                    ( Padding(Max, Pad) )
import           Brick.Widgets.List             ( GenericList
                                                , renderList
                                                )
import           Data.Sequence                  ( Seq )
import           Types.Brick.Name
import           Types.Classes.Displayable

type ZZZList x = GenericList Name Seq x

renderGenericList :: Displayable x => ZZZList x -> Widget Name
renderGenericList = renderList f True
 where
  f selected item =
    let mainLine = padRight Max $ txt $ display item
    in  (if selected then txt "❯ " <+> mainLine else padLeft (Pad 2) mainLine)
