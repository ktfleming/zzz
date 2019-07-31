{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module UI.EventHandlers.ActiveList where

import           Lens.Micro.Platform            ( (.~) )
import           Types.AppState
import           Types.Project
import           Types.Screen
import           UI.List                        ( ZZZList )

class ListSelectable a where
  onSelect :: AppState -> a -> AppState

-- Similar to `ActiveForm`, but for Brick lists
data ActiveList = forall x. ListSelectable x => ActiveList (ZZZList x)

instance ListSelectable Project where
  onSelect :: AppState -> Project -> AppState
  onSelect s p = (activeScreen .~ ProjectDetailsScreen p) s

instance Show ActiveList where
  show _ = "(List is active)"
