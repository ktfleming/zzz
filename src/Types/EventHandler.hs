{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Types.EventHandler where

import           Brick.Forms                    ( formState )
import           Lens.Micro.Platform            ( (.~)
                                                , (<>~)
                                                )
import           Types.AppState                 ( AppState(..)
                                                , activeScreen
                                                , allProjects
                                                )
import           Types.Project
import           Types.Screen
import           UI.Form                        ( ZZZForm )
import           UI.List                        ( ZZZList )
import           UI.Projects.Add                ( ProjectAddState(..) )


class ListSelectable a where
  onSelect :: AppState -> a -> AppState

-- Similar to `ActiveForm`, but for Brick lists
data ActiveList = forall x. ListSelectable x => ActiveList (ZZZList x)


instance ListSelectable Project where
  onSelect :: AppState -> Project -> AppState
  onSelect s p = (activeScreen .~ ProjectDetailsScreen p) s

instance Show ActiveList where
  show _ = "(List is active)"
