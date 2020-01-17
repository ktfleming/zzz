{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Specs.Navigation.RequestDefs
  ( requestDefNavTree,
  )
where

import Brick.Focus (focusGetCurrent)
import Brick.Forms (allFieldsValid)
import Brick.Widgets.List (listSelected)
import Control.Lens
import qualified Data.HashMap.Strict as Map
import Data.Singletons
  ( fromSing,
  )
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Hedgehog
  ( (===),
    PropertyT,
    cover,
    failure,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import TestUtils
import Types.AppState
import Types.Brick.Name
import Types.Classes.Fields
import Types.Modal (Modal (DeleteRequestDefModal, DeleteResponseModal))
import Types.Models.Id (ProjectId)
import Types.Models.Project (ProjectContext (..), requestDefs)
import Types.Models.RequestDef
import Types.Models.Response (ResponseIndex (..))
import Types.Models.Screen
import Types.Models.Screen.Optics
import Types.Models.Url
import UI.FocusRing (AppFocusRing (..))
import UI.Form (AppForm (..))
import UI.List (AppList (..))

-- Make sure that no RequestDefs have an empty name or url, which should
-- be caught by the validation functions on the forms
validateRequestDefs :: Monad m => AnyAppState -> PropertyT m ()
validateRequestDefs s = do
  let allRds = allRequestDefs s
  any (T.null . view (name . coerced)) allRds === False
  any (T.null . view (url . coerced)) allRds === False

countRequestDefs :: AnyAppState -> ProjectId -> Int
countRequestDefs s pid = Map.size (s ^. projects . ix pid . requestDefs)

requestDefNavTree :: TestTree
requestDefNavTree =
  testGroup
    "RequestDef Navigation"
    [ testGroup
        "RequestDefAddScreen"
        [ prop "Pressing Ctrl+s" $ do
            i@(AnyAppState SRequestDefAddTag initial@AppState {appStateScreen = RequestDefAddScreen (ProjectContext pid) (AppForm form)}) <- with RequestDefAddTag
            n@(AnyAppState newTag _) <- getNextState initial (KChar 's') [MCtrl]
            let valid = allFieldsValid form
            cover 50 "Valid" valid
            cover 2 "Invalid" $ not valid
            if valid
              then do
                fromSing newTag === ProjectDetailsTag
                countRequestDefs n pid === countRequestDefs i pid + 1
              else do
                n === i
                validateRequestDefs n,
          propN 1 "Pressing ESC" $ do
            i@(AnyAppState SRequestDefAddTag initial@AppState {appStateScreen = RequestDefAddScreen c _}) <- with RequestDefAddTag
            n@(AnyAppState SProjectDetailsTag AppState {appStateScreen = ProjectDetailsScreen newC _}) <- getNextState initial KEsc []
            newC === c
            n ^. projects === i ^. projects
        ],
      testGroup
        "RequestDefEditScreen"
        [ prop "Pressing Ctrl+s" $ do
            i@(AnyAppState SRequestDefEditTag initial@AppState {appStateScreen = RequestDefEditScreen (RequestDefContext pid _) (AppForm form)}) <- with RequestDefEditTag
            n@(AnyAppState newTag _) <- getNextState initial (KChar 's') [MCtrl]
            let valid = allFieldsValid form
            cover 50 "Valid" valid
            cover 2 "Invalid" $ not valid
            if valid
              then do
                fromSing newTag === RequestDefDetailsTag
                countRequestDefs n pid === countRequestDefs i pid
                validateRequestDefs n
              else n === i,
          propN 1 "Pressing ESC" $ do
            i@(AnyAppState SRequestDefEditTag initial@AppState {appStateScreen = RequestDefEditScreen c _}) <- with RequestDefEditTag
            n@(AnyAppState SRequestDefDetailsTag newState) <- getNextState initial KEsc []
            newState ^. screen ^. context === c
            n ^. projects === i ^. projects
        ],
      testGroup
        "RequestDefDetailsScreen"
        [ prop "Pressing CTRL+e" $ do
            (AnyAppState SRequestDefDetailsTag i) <- with RequestDefDetailsTag
            (AnyAppState SRequestDefEditTag n) <- getNextState i (KChar 'e') [MCtrl]
            n ^. screen ^. context === i ^. screen ^. context
            n ^. projects === i ^. projects,
          prop "Pressing CTRL+d" $ do
            i@(AnyAppState SRequestDefDetailsTag AppState {appStateScreen = RequestDefDetailsScreen c (AppList list) (AppFocusRing ring) _ _}) <- with RequestDefDetailsTag
            cover 10 "RequestDetails selected" $ focusGetCurrent ring == Just RequestDetails
            cover 10 "ResponseList selected" $ focusGetCurrent ring == Just ResponseList
            cover 10 "ResponseBodyDetails selected" $ focusGetCurrent ring == Just ResponseBodyDetails
            n <- getNextState' i (KChar 'd') [MCtrl]
            case (focusGetCurrent ring, listSelected list) of
              (Just RequestDetails, _) -> n ^. modal === Just (DeleteRequestDefModal c)
              (Just ResponseList, Just idx) -> n ^. modal === Just (DeleteResponseModal c (ResponseIndex idx))
              (Just ResponseBodyDetails, Just idx) -> n ^. modal === Just (DeleteResponseModal c (ResponseIndex idx))
              _ -> failure
            n ^. projects === i ^. projects,
          prop "Pressing the escape key" $ do
            i@(AnyAppState SRequestDefDetailsTag initial@AppState {appStateScreen = RequestDefDetailsScreen (RequestDefContext pid _) _ _ _ _}) <- with RequestDefDetailsTag
            n@(AnyAppState SProjectDetailsTag AppState {appStateScreen = ProjectDetailsScreen (ProjectContext newPid) _}) <- getNextState initial KEsc []
            pid === newPid
            n ^. projects === i ^. projects
        ]
    ]
