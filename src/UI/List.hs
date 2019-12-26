{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.List where

import Brick
  ( (<+>),
    Widget,
    padLeft,
    padRight,
    txt,
  )
import Brick.Types (Padding (Max, Pad))
import Brick.Widgets.List
  ( GenericList,
    listElements,
    renderList,
  )
import Data.Sequence (Seq)
import Types.Brick.Name
import Types.Classes.Displayable

newtype AppList a = AppList (GenericList Name Seq a) deriving (Show, Functor)

instance Eq a => Eq (AppList a) where
  AppList list1 == AppList list2 = listElements list1 == listElements list2

renderGenericList :: Displayable x => Bool -> Bool -> AppList x -> Widget Name
renderGenericList focused showSelection (AppList list) = renderList f focused list
  where
    f selected item =
      let mainLine = padRight Max $ display item
       in (if selected && showSelection then txt "❯ " <+> mainLine else padLeft (Pad 2) mainLine)
