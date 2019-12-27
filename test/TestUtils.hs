{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module TestUtils where

import Brick.BChan (newBChan)
import Brick.Types (BrickEvent (VtyEvent))
import Brick.Widgets.List (listSelected)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as Map
import Data.Maybe (isJust)
import Data.Singletons (SingI, fromSing, sing, withSingI)
import Gens
import Graphics.Vty (Key, Modifier)
import Graphics.Vty.Input.Events
import Hedgehog
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import TestMonad (runTestM)
import Types.AppState (AnyAppState (..), AppState)
import Types.Classes.Fields
import Types.Config.Config
import Types.Models.Project (Project, requestDefs)
import Types.Models.RequestDef
import Types.Models.Screen (ScreenTag (..))
import Types.Models.Screen.Optics
import UI.Events.Handler (handleEvent)
import UI.List (AppList (..))

-- Shortcut to make tests less verbose
prop :: String -> PropertyT IO () -> TestTree
prop desc = testProperty desc . property

propN :: TestLimit -> String -> PropertyT IO () -> TestTree
propN limit desc = testProperty desc . withTests limit . property

-- Given an initial tagged AppState and a BrickEvent, use the event handler to
-- process the event and return the resulting AppState (wrapped in AnyAppState)
getNextState :: (SingI a, MonadIO m) => AppState a -> Key -> [Modifier] -> m AnyAppState
getNextState s = getNextState' (AnyAppState sing s)

-- Same as getNextState but accepts AnyAppState to start with
getNextState' :: MonadIO m => AnyAppState -> Key -> [Modifier] -> m AnyAppState
getNextState' s key mods =
  let testM = liftIO (newBChan 5) >>= \chan -> handleEvent chan s (VtyEvent (EvKey key mods))
   in liftIO $ runReaderT (runTestM testM) defaultConfig

-- Given a key (and modifiers) to press, an expected ScreenTag, and an initial AppState,
-- assert that handling the keypress event will result in the expected screen being placed in the state
check :: (MonadTest m, MonadIO m) => Key -> [Modifier] -> ScreenTag -> AnyAppState -> m ()
check key mods expectedTag (AnyAppState tag initial) = withSingI tag $ do
  AnyAppState newTag _ <- getNextState initial key mods
  fromSing newTag === expectedTag

-- Gets all the request definitions from an AppState
allRequestDefs :: AnyAppState -> [RequestDef]
allRequestDefs s = Map.foldr fn [] (s ^. projects)
  where
    fn :: Project -> [RequestDef] -> [RequestDef]
    fn p rds = rds <> Map.elems (p ^. requestDefs)

{- Some predicates to use when doing tests -}

-- Whether or not at least one Project is available
hasProject :: AnyAppState -> Bool
hasProject s = (not . Map.null) $ s ^. projects

-- Whether or not the currently displayed Project has at least one RequestDef
requestDefSelected :: AppState 'ProjectDetailsTag -> Bool
requestDefSelected s =
  let AppList list = s ^. listLens
   in isJust $ listSelected list

-- Just a shortcut since so many specs start with this kind of `forAll`
with :: Monad m => ScreenTag -> PropertyT m AnyAppState
with tag = forAll (genAppState tag)
