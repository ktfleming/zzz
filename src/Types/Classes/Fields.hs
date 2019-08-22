{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Types.Classes.Fields where

import           Control.Lens

class HasName s a | s -> a where
  name :: Lens' s a

class HasUrl s a | s -> a where
  url :: Lens' s a

class HasHeaders s a | s -> a where
  headers :: Lens' s a
