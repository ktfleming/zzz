{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Specs.NavigationSpec
  ( navTestTree,
  )
where

import Brick.BChan (newBChan)
import Brick.Types (BrickEvent (VtyEvent))
import Brick.Widgets.List (listSelected)
import Control.Lens
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Control.Monad.Indexed ((>>>=))
import Control.Monad.Indexed.State (runIxStateT)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (isJust)
import Data.Singletons
  ( SingI,
    fromSing,
    sing,
    withSingI,
  )
import Gens
import Graphics.Vty.Input.Events
import Hedgehog
  ( (===),
    MonadTest,
    forAll,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import TestMonad
import TestUtils (prop)
import Types.AppState
import Types.Brick.CustomEvent
import Types.Brick.Name
import Types.Classes.Fields
import Types.Models.Screen
import Types.Monads
import UI.Events.Handler (handleEventInState)
import UI.List (AppList (..))

-- Given an initial tagged AppState and a BrickEvent, use the event handler to
-- process the event and return the resulting AppState (wrapped in AnyAppState)
getNextState :: (SingI a, MonadIO m) => AppState a -> BrickEvent Name CustomEvent -> m AnyAppState
getNextState s event =
  let anyS = AnyAppState sing s
      testM = iliftIO (newBChan 5) >>>= handleEventInState event
   in liftIO $ snd <$> (runIxStateT . runTestM) testM anyS

-- Given a key (and modifiers) to press, an expected ScreenTag, and an initial AppState,
-- assert that handling the keypress event will result in the expected screen being placed in the state
check :: (MonadTest m, MonadIO m) => Key -> [Modifier] -> ScreenTag -> AnyAppState -> m ()
check key mods expectedTag (AnyAppState tag initial) = withSingI tag $ do
  AnyAppState newTag _ <- getNextState initial (VtyEvent (EvKey key mods))
  fromSing newTag === expectedTag

{- Some predicates to use when doing tests -}

-- Whether or not at least one Project is available
hasProject :: AnyAppState -> Bool
hasProject s = (not . Map.null) $ s ^. projects

-- Whether or not the current screen is the ProjectDetailsScreen with a RequestDef selected
-- (i.e. the currently displayed Project has at least one RequestDef)
requestDefSelected :: AnyAppState -> Bool
requestDefSelected (AnyAppState _ s) = case s ^. screen of
  ProjectDetailsScreen _ (AppList list) -> isJust $ listSelected list
  _ -> False

navTestTree :: TestTree
navTestTree =
  testGroup
    "Navigation"
    [ testGroup
        "ProjectListScreen"
        [ prop "Pressing the 'a' key" $ do
            initial <- forAll $ genAppState ProjectListTag
            check (KChar 'a') [] ProjectAddTag initial,
          prop "Pressing the right arrow key" $ do
            initial <- forAll $ genAppState ProjectListTag
            let expected =
                  if hasProject initial
                    then ProjectDetailsTag
                    else ProjectListTag
            check KRight [] expected initial
        ],
      testGroup
        "ProjectDetailsScreen"
        [ prop "Pressing the right arrow key" $ do
            initial <- forAll $ genAppState ProjectDetailsTag
            let expected =
                  if requestDefSelected initial
                    then RequestDefDetailsTag
                    else ProjectDetailsTag
            check KRight [] expected initial
        ]
    ]
