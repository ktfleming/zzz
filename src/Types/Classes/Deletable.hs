{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types.Classes.Deletable where

import qualified Data.Text as T
import Types.Classes.WithID (HasID, Context)
import Types.AppState
import Types.Models.Project (Project, ProjectContext(..), Project(..), requestDefinitions)
import Lens.Micro.Platform ((.~), (&), at, _Just)
import Types.Models.RequestDefinition (RequestDefinition(..), RequestDefinitionContext(..), _requestDefinitionName)

class HasID a => Deletable a where
  delete :: AppState -> Context a -> AppState
  warning :: AppState -> Context a -> T.Text
  
instance Deletable Project where
  delete :: AppState -> ProjectContext -> AppState
  delete s (ProjectContext pid) = s & projects . at pid .~ Nothing
  
  warning s c =
    let Project { _projectName } = lookupProject s c
    in "Are you sure you want to delete project '" <> _projectName <> "'? All contained request definitions will also be deleted!"
    
instance Deletable RequestDefinition where
  delete :: AppState -> RequestDefinitionContext -> AppState
  delete s (RequestDefinitionContext pid rid) = s & projects . at pid . _Just . requestDefinitions . at rid .~ Nothing
  
  warning s c =
    let RequestDefinition { _requestDefinitionName } = lookupRequestDefinition s c
    in "Are you sure you want to delete request definition '" <> _requestDefinitionName <> "'?"