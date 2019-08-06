{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module UI.EventHandlers.ActiveList where

import           UI.List                        ( ZZZList )
import           UI.ShowDetails                 ( ShowDetails )
import           Types.WithID                   ( HasID )
import           Types.Project                  ( Project
                                                , ProjectListItem
                                                )
import           Types.RequestDefinition        ( RequestDefinition
                                                , RequestDefinitionListItem
                                                )

class HasID a => Listable a where
  type family ListItem a = b | b -> a -- The type that represents an `a` shown in a list

-- Similar to `ActiveForm`, but for Brick lists. Any item in the list must
-- be able to have its details shown to the user.
data ActiveList = forall a. (ShowDetails a, Listable a) => ActiveList (ZZZList a)

instance Listable Project where
  type ListItem Project = ProjectListItem

instance Listable RequestDefinition where
  type ListItem RequestDefinition = RequestDefinitionListItem

instance Show ActiveList where
  show _ = "(List is active)"
