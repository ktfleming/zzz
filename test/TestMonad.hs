{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module TestMonad where

import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Control.Monad.Indexed
  ( IxApplicative,
    IxFunctor,
    IxMonad,
    IxPointed,
  )
import Control.Monad.Indexed.State
  ( IxMonadState,
    IxStateT,
  )
import Types.Monads
  ( IxMonadEvent,
    IxMonadIO,
    iliftEvent,
    iliftIO,
  )

-- A monad to run the tests in -- similar to the "real" app monad AppM,
-- but does not include Brick's EventM in the stack. This is because running in
-- EventM seems to require actual terminal capabilities, making it harder to test.
-- Note that not having EventM means that we can't test Brick's even handlers that update
-- lists and forms.
newtype TestM i o a
  = TestM
      { runTestM :: IxStateT IO i o a
      }
  deriving (IxPointed, IxFunctor, IxApplicative, IxMonad, IxMonadState)

deriving instance Functor (TestM i i)

deriving instance Applicative (TestM i i)

deriving instance Monad (TestM i i)

deriving instance MonadIO (TestM i i)

-- We can "lift" computations in EventM into TestM by just ignoring the EventM value
-- and using the provided "default" value, which in practice is just the initial state
-- of the form/list. This means that our forms/lists will not be modified.
instance IxMonadEvent TestM where
  iliftEvent x _ = return x

instance IxMonadIO TestM where
  iliftIO = liftIO
