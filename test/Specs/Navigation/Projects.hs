{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Specs.Navigation.Projects
  ( projectNavTestTree,
  )
where

import Brick.Forms (allFieldsValid)
import Control.Lens
import qualified Data.HashMap.Strict as Map
import Data.Singletons
  ( fromSing,
  )
import Graphics.Vty.Input.Events
import Hedgehog
  ( (===),
    cover,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import TestUtils
import Types.AppState
import Types.Classes.Fields
import Types.Modal (Modal (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import UI.Form (AppForm (..))

projectNavTestTree :: TestTree
projectNavTestTree =
  testGroup
    "Project Navigation"
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
            i@(AnyAppState SProjectDetailsTag initial) <- with ProjectDetailsTag
            let expected =
                  if requestDefSelected initial
                    then RequestDefDetailsTag
                    else ProjectDetailsTag
            check KRight [] expected i,
          propN 1 "Pressing the left arrow key" $
            with ProjectDetailsTag >>= check KLeft [] ProjectListTag,
          propN 1 "Pressing the 'e' key" $
            with ProjectDetailsTag >>= check (KChar 'e') [] ProjectEditTag,
          propN 1 "Pressing the 'a' key" $
            with ProjectDetailsTag >>= check (KChar 'a') [] RequestDefAddTag,
          prop "Pressing the 'd' key" $ do
            i@(AnyAppState SProjectDetailsTag AppState {appStateScreen = ProjectDetailsScreen c _}) <- with ProjectDetailsTag
            n <- getNextState' i (KChar 'd') []
            n ^. modal === Just (DeleteProjectModal c)
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
