{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Specs.Navigation.Environments
  ( environmentNavTree,
  )
where

import Brick.Forms (allFieldsValid)
import Brick.Widgets.List (listSelectedElement)
import Control.Lens
import qualified Data.HashMap.Strict as Map
import Data.Singletons
  ( fromSing,
  )
import Graphics.Vty.Input.Events
import Hedgehog ((===), cover, failure)
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import TestUtils
import Types.AppState
import Types.Classes.Fields
import Types.Modal
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Search
import UI.Form (AppForm (..))
import UI.List (AppList (..))
import UI.RequestDefs.Details (makeResponseList)

environmentNavTree :: TestTree
environmentNavTree =
  testGroup
    "Environment Navigation"
    [ testGroup
        "EnvironmentAddScreen"
        [ prop "Pressing Ctrl-s" $ do
            i@( AnyAppState
                  SEnvironmentAddTag
                  initial@AppState
                    { appStateScreen = EnvironmentAddScreen (AppForm form)
                    }
                ) <-
              with EnvironmentAddTag
            n@(AnyAppState newTag _) <- getNextState initial (KChar 's') [MCtrl]
            let valid = allFieldsValid form
            cover 50 "Valid" valid
            cover 2 "Invalid" $ not valid
            if valid
              then do
                fromSing newTag === EnvironmentListTag
                Map.size (n ^. environments) === Map.size (i ^. environments) + 1
              else n === i,
          propN 1 "Pressing ESC" $ do
            i@(AnyAppState SEnvironmentAddTag _) <- with EnvironmentAddTag
            (AnyAppState newTag _) <- getNextState' i KEsc []
            fromSing newTag === fromSing SEnvironmentListTag
        ],
      testGroup
        "EnvironmentListScreen"
        [ prop "Pressing Enter" $ do
            ( AnyAppState
                SEnvironmentListTag
                initial@AppState
                  { _appStateStashedScreen = Just (AnyScreen initialStashedTag initialStashedScreen)
                  }
              ) <-
              with EnvironmentListTag
            (AnyAppState newTag newState) <- getNextState initial KEnter []
            -- Selecting an environment should unstash the stashed screen, leaving nothing stashed
            newState ^. stashedScreen === Nothing
            let AppList list = initial ^. screen ^. searchTools ^. appList
            selectedEnv <- case listSelectedElement list of
              Just (_, SelectableResult NoEnvironmentResult) -> pure Nothing
              Just (_, (SelectableResult (AnEnvironmentResult ec _))) -> pure $ Just ec
              _ -> failure -- there should always be an item selected in the list

            -- The environment that was selected should be reflected in the new state
            newState ^. environmentContext === selectedEnv
            -- If the stashed screen was the RequestDef details screen, the list of Responses in the
            -- screen once it's unstashed should be changed if the environment changed, since they
            -- have to be refreshed
            let newScreen = AnyScreen newTag (newState ^. screen)
                expectedScreen = case initialStashedScreen of
                  RequestDefDetailsScreen c _ ring err ->
                    -- Look up the environment that was selected, the request def that is being displayed, and
                    -- the responses that belong to that pair
                    let newResponses = lookupResponses newState c (currentEnvironmentKey newState)
                     in RequestDefDetailsScreen c (makeResponseList newResponses) ring err
                  _ -> initialStashedScreen
            newScreen === AnyScreen initialStashedTag expectedScreen,
          prop "Pressing CTRL+d" $ do
            i@(AnyAppState SEnvironmentListTag initial) <- with EnvironmentListTag
            n <- getNextState initial (KChar 'd') [MCtrl]
            let AppList list = initial ^. screen ^. searchTools ^. appList
            case listSelectedElement list of
              Just (_, SelectableResult NoEnvironmentResult) -> i === n -- nothing selected, so no modal appears
              Just (_, SelectableResult (AnEnvironmentResult ec _)) -> n ^. modal === Just (DeleteEnvironmentModal ec)
              _ -> failure,
          prop "Pressing CTRL+e" $ do
            i@(AnyAppState SEnvironmentListTag initial) <- with EnvironmentListTag
            n@(AnyAppState newTag newState) <- getNextState initial (KChar 'e') [MCtrl]
            let AppList list = initial ^. screen ^. searchTools ^. appList
            case listSelectedElement list of
              Just (_, (SelectableResult NoEnvironmentResult)) -> i === n -- nothing selected, so nothing happens
              Just (_, (SelectableResult (AnEnvironmentResult ec _))) -> do
                fromSing newTag === EnvironmentEditTag
                case newState ^. screen of
                  EnvironmentEditScreen ec' _ -> ec' === ec
                  _ -> failure
              _ -> failure,
          propN 1 "Pressing CTRL+a" $ do
            i <- with EnvironmentListTag
            check (KChar 'a') [MCtrl] EnvironmentAddTag i
        ],
      testGroup
        "Environment edit screen"
        [ prop "Pressing Ctrl-s" $ do
            i@(AnyAppState SEnvironmentEditTag initial) <- with EnvironmentEditTag
            n@(AnyAppState newTag _) <- getNextState initial (KChar 's') [MCtrl]
            let AppForm form = initial ^. formLens
                valid = allFieldsValid form
            cover 50 "Valid" valid
            cover 2 "Invalid" $ not valid
            if valid
              then do
                fromSing newTag === EnvironmentListTag
                Map.size (n ^. environments) === Map.size (i ^. environments)
              else i === n,
          propN 1 "Pressing ESC" $ do
            i@(AnyAppState SEnvironmentEditTag _) <- with EnvironmentEditTag
            (AnyAppState newTag _) <- getNextState' i KEsc []
            fromSing newTag === fromSing SEnvironmentListTag
        ]
    ]
