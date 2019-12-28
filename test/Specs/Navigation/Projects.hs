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
        [ propN 1 "Pressing CTRL+a" $
            with ProjectListTag >>= check (KChar 'a') [MCtrl] ProjectAddTag,
          prop "the enter key" $ do
            s <- with ProjectListTag
            let expected =
                  if hasProject s
                    then ProjectDetailsTag
                    else ProjectListTag
            check KEnter [] expected s
        ],
      testGroup
        "ProjectDetailsScreen"
        [ prop "Pressing the enter key" $ do
            i@(AnyAppState SProjectDetailsTag initial) <- with ProjectDetailsTag
            let expected =
                  if requestDefSelected initial
                    then RequestDefDetailsTag
                    else ProjectDetailsTag
            check KEnter [] expected i,
          propN 1 "Pressing the escape key" $
            with ProjectDetailsTag >>= check KEsc [] ProjectListTag,
          propN 1 "Pressing CTRL+e" $
            with ProjectDetailsTag >>= check (KChar 'e') [MCtrl] ProjectEditTag,
          propN 1 "Pressing CTRL+a" $
            with ProjectDetailsTag >>= check (KChar 'a') [MCtrl] RequestDefAddTag,
          prop "Pressing CTRL+d" $ do
            i@(AnyAppState SProjectDetailsTag AppState {appStateScreen = ProjectDetailsScreen c _}) <- with ProjectDetailsTag
            n <- getNextState' i (KChar 'd') [MCtrl]
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
