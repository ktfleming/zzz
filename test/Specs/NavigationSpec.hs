{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Specs.NavigationSpec
  ( navTestTree,
  )
where

import Brick.BChan (newBChan)
import Brick.Forms (allFieldsValid)
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
    PropertyT,
    cover,
    failure,
    forAll,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import TestMonad
import TestUtils (prop, propN)
import Types.AppState
import Types.Classes.Fields
import Types.Modal (Modal (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Monads
import UI.Events.Handler (handleEventInState)
import UI.Form (AppForm (..))
import UI.List (AppList (..))

-- Given an initial tagged AppState and a BrickEvent, use the event handler to
-- process the event and return the resulting AppState (wrapped in AnyAppState)
getNextState :: (SingI a, MonadIO m) => AppState a -> Key -> [Modifier] -> m AnyAppState
getNextState s = getNextState' (AnyAppState sing s)

-- Same as getNextState but accepts AnyAppState to start with
getNextState' :: MonadIO m => AnyAppState -> Key -> [Modifier] -> m AnyAppState
getNextState' s key mods =
  let testM = iliftIO (newBChan 5) >>>= handleEventInState (VtyEvent (EvKey key mods))
   in liftIO $ snd <$> (runIxStateT . runTestM) testM s

-- Given a key (and modifiers) to press, an expected ScreenTag, and an initial AppState,
-- assert that handling the keypress event will result in the expected screen being placed in the state
check :: (MonadTest m, MonadIO m) => Key -> [Modifier] -> ScreenTag -> AnyAppState -> m ()
check key mods expectedTag (AnyAppState tag initial) = withSingI tag $ do
  AnyAppState newTag _ <- getNextState initial key mods
  fromSing newTag === expectedTag

{- Some predicates to use when doing tests -}

-- Whether or not at least one Project is available
hasProject :: AnyAppState -> Bool
hasProject s = (not . Map.null) $ s ^. projects

-- Whether or not the currently displayed Project has at least one RequestDef
requestDefSelected :: AppState 'ProjectDetailsTag -> Bool
requestDefSelected s =
  let AppList list = s ^. listLens
   in isJust $ listSelected list

with :: Monad m => ScreenTag -> PropertyT m AnyAppState
with tag = forAll (genAppState tag)

navTestTree :: TestTree
navTestTree =
  testGroup
    "Navigation"
    [ testGroup
        "ProjectListScreen"
        [ propN 1 "Pressing the 'a' key" $
            with ProjectListTag >>= check (KChar 'a') [] ProjectAddTag,
          prop "the right arrow key" $ do
            s <- with ProjectListTag
            let expected =
                  if hasProject s
                    then ProjectDetailsTag
                    else ProjectListTag
            check KRight [] expected s
        ],
      testGroup
        "ProjectDetailsScreen"
        [ prop "Pressing the right arrow key" $ do
            s@(AnyAppState SProjectDetailsTag initial) <- with ProjectDetailsTag
            let expected =
                  if requestDefSelected initial
                    then RequestDefDetailsTag
                    else ProjectDetailsTag
            check KRight [] expected s,
          propN 1 "Pressing the left arrow key" $
            with ProjectDetailsTag >>= check KLeft [] ProjectListTag,
          propN 1 "Pressing the 'e' key" $
            with ProjectDetailsTag >>= check (KChar 'e') [] ProjectEditTag,
          propN 1 "Pressing the 'a' key" $
            with ProjectDetailsTag >>= check (KChar 'a') [] RequestDefAddTag,
          prop "Pressing the 'd' key" $ do
            initial <- with ProjectDetailsTag
            (AnyAppState SProjectDetailsTag newS) <- getNextState' initial (KChar 'd') []
            case newS ^. screen of
              ProjectDetailsScreen c _ -> newS ^. modal === Just (DeleteProjectModal c)
              _ -> failure
        ],
      testGroup
        "ProjectAddScreen"
        [ prop "Pressing CTRL+s" $ do
            i@(AnyAppState SProjectAddTag initial) <- with ProjectAddTag
            n@(AnyAppState newTag _) <- getNextState initial (KChar 's') [MCtrl]
            let AppForm form = initial ^. formLens
                valid = allFieldsValid form
            cover 50 "Valid" valid
            cover 2 "Invalid" $ not valid
            if valid
              then do
                fromSing newTag === ProjectListTag
                Map.size (n ^. projects) === Map.size (i ^. projects) + 1
              else i === n, -- Form is invalid; state is completely unchanged
          propN 1 "Pressing ESC" $
            with ProjectAddTag >>= check KEsc [] ProjectListTag
        ],
      testGroup
        "ProjectEditScreen"
        [ prop "Pressing CTRL+s" $ do
            i@(AnyAppState SProjectEditTag initial) <- with ProjectEditTag
            n@(AnyAppState newTag _) <- getNextState initial (KChar 's') [MCtrl]
            let AppForm form = initial ^. formLens
                valid = allFieldsValid form
            cover 50 "Valid" valid
            cover 2 "Invalid" $ not valid
            if valid
              then do
                fromSing newTag === ProjectDetailsTag
                Map.size (n ^. projects) === Map.size (i ^. projects)
              else i === n,
          propN 1 "Pressing ESC" $
            with ProjectEditTag >>= check KEsc [] ProjectDetailsTag
        ]
    ]
