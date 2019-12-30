{-# LANGUAGE FunctionalDependencies #-}

module Types.Classes.Fields where

import Control.Lens

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

class HasCurrentTime s a | s -> a where
  currentTime :: Lens' s a

class HasModal s a | s -> a where
  modal :: Lens' s a

class HasProjects s a | s -> a where
  projects :: Lens' s a

class HasEnvironments s a | s -> a where
  environments :: Lens' s a

class HasStashedScreen s a | s -> a where
  stashedScreen :: Lens' s a

class HasQuit s a | s -> a where
  quit :: Lens' s a

class HasSave s a | s -> a where
  save :: Lens' s a

class HasBack s a | s -> a where
  back :: Lens' s a

class HasAdd s a | s -> a where
  add :: Lens' s a

class HasDelete s a | s -> a where
  delete :: Lens' s a

class HasEdit s a | s -> a where
  edit :: Lens' s a

class HasCancel s a | s -> a where
  cancel :: Lens' s a

class HasSubmit s a | s -> a where
  submit :: Lens' s a

class HasFocusPrev s a | s -> a where
  focusPrev :: Lens' s a

class HasFocusNext s a | s -> a where
  focusNext :: Lens' s a

class HasScrollUp s a | s -> a where
  scrollUp :: Lens' s a

class HasScrollDown s a | s -> a where
  scrollDown :: Lens' s a

class HasShowHelp s a | s -> a where
  showHelp :: Lens' s a

class HasSearchAll s a | s -> a where
  searchAll :: Lens' s a

class HasShowEnvironments s a | s -> a where
  showEnvironments :: Lens' s a

class HasKeymap s a | s -> a where
  keymap :: Lens' s a
