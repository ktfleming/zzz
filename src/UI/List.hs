{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module UI.List where

import           Brick                          ( Widget
                                                , txt
                                                , padRight
                                                )
import           Brick.Widgets.List             ( GenericList
                                                , renderList
                                                )
import           Data.Vector                    ( Vector )
import           Types.Name
import           Types.Displayable
import           Brick.Types                    ( Padding(Max) )

type ZZZList x = GenericList Name Vector x

renderGenericList :: Displayable x => ZZZList x -> Widget Name
renderGenericList = renderList f True
  where f _ item = padRight Max $ txt $ display item
