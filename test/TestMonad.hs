{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module TestMonad where

import Control.Monad.IO.Class
  ( MonadIO,
  )
import Control.Monad.Reader
import Types.Config.Config
import Types.Monads

-- A monad to run the tests in -- similar to the "real" app monad AppM,
-- but does not include Brick's EventM in the stack. This is because running in
-- EventM seems to require actual terminal capabilities, making it harder to test.
-- Note that not having EventM means that we can't test Brick's even handlers that update
-- lists and forms.
newtype TestM a
  = TestM
      { runTestM :: ReaderT Config IO a
      }
  deriving (Functor, Applicative, Monad, MonadIO)

-- We can "lift" computations in EventM into TestM by just ignoring the EventM value
-- and using the provided "default" value, which in practice is just the initial state
-- of the form/list. This means that our forms/lists will not be modified.
instance MonadEvent TestM where
  liftEvent x _ = pure x

deriving instance MonadReader Config TestM
