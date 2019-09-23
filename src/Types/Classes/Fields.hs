{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Types.Classes.Fields where

import           Control.Lens

-- These are written out by hand so that makeFields from lens will use these typeclasses
-- instead of generating ones in other files via TemplateHaskell; this makes it easier to
-- avoid import/dependency problems.

class HasName s a | s -> a where
  name :: Lens' s a

class HasUrl s a | s -> a where
  url :: Lens' s a

class HasHeaders s a | s -> a where
  headers :: Lens' s a

class HasBody s a | s -> a where
  body :: Lens' s a

class HasMethod s a | s -> a where
  method :: Lens' s a

class HasValue s a | s -> a where
  value :: Lens' s a

class HasVariables s a | s -> a where
  variables :: Lens' s a

class HasDateTime s a | s -> a where
  dateTime :: Lens' s a

class HasText s a | s -> a where
  text :: Lens' s a
