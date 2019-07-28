{-# LANGUAGE ExistentialQuantification #-}
module UI.List where

import           Brick                          ( Widget
                                                , str
                                                )
import           Brick.Widgets.List             ( GenericList
                                                , renderList
                                                )
import           Data.Vector                    ( Vector )
import           Types.Name

renderGenericList
  :: forall x . Show x => GenericList Name Vector x -> Widget Name
renderGenericList = renderList renderFn True
  where renderFn _ item = str $ show item
