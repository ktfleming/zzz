{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Gens where

import Brick.Focus (focusRing, focusSetCurrent)
import Control.Lens
import Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Singletons (sing)
import Data.Time (NominalDiffTime, UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.IO (unsafePerformIO)
import Graphics.Vty (Key (KChar), Modifier (MCtrl, MMeta))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Types.AppState
  ( AnyAppState (..),
    AppState (..),
    EnvironmentKey (..),
    HelpPanelVisible (..),
    Responses,
  )
import Types.Brick.Name
import Types.Classes.Fields (variables)
import Types.Methods (Method (..))
import Types.Models.Environment
import Types.Models.Header
import Types.Models.Id
import Types.Models.Project
import Types.Models.RequestDef
import Types.Models.Response
import Types.Models.Screen
import Types.Models.Url (Url (..))
import UI.Environments.Common (makeEnvironmentForm)
import UI.Environments.List (makeEnvironmentList)
import UI.FocusRing (AppFocusRing (..))
import UI.Projects.Common (makeProjectForm)
import UI.Projects.Details (makeRequestDefList)
import UI.Projects.List (makeProjectList)
import UI.RequestDefs.Common (makeRequestDefForm)
import UI.RequestDefs.Details (makeResponseList)

genUUID :: Gen UUID
genUUID = Gen.constant $ unsafePerformIO nextRandom

genMethod :: Gen Method
genMethod = Gen.element [Get, Post, Put, Patch, Delete]

genUrl :: Gen Url
genUrl = Url <$> Gen.text (Range.linear 8 50) Gen.alpha -- TODO: make it more like a real URL?

-- TODO: make more like a real request body. Also combine this with genMethod so, for example, GET requests
-- don't have bodies
genRequestBody :: Gen RequestBody
genRequestBody = RequestBody <$> Gen.text (Range.linear 0 100) Gen.alpha

genHeader :: Gen Header
genHeader = do
  name <- HeaderName <$> Gen.text (Range.linear 1 15) Gen.alpha
  value <- HeaderValue <$> Gen.text (Range.linear 1 15) Gen.alpha
  return $ Header name value

genRequestDefs :: Gen (HashMap RequestDefId RequestDef)
genRequestDefs = fmap Map.fromList $ Gen.list (Range.linear 0 20) $ do
  rid <- RequestDefId <$> genUUID
  name <- RequestDefName <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  url <- genUrl
  method <- genMethod
  body <- genRequestBody
  headers <- Gen.seq (Range.linear 0 10) genHeader
  return (rid, RequestDef name url method body headers)

genProject :: Gen Project
genProject = do
  name <- ProjectName <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  Project name <$> genRequestDefs

genProjects :: Gen (HashMap ProjectId Project)
genProjects = fmap Map.fromList $ Gen.list (Range.linear 0 20) $ do
  pid <- ProjectId <$> genUUID
  project <- genProject
  return (pid, project)

genVariable :: Gen Variable
genVariable = do
  name <- VariableName <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  value <- VariableValue <$> Gen.text (Range.linear 1 30) Gen.alphaNum
  return $ Variable name value

genEnvironment :: Gen Environment
genEnvironment = do
  name <- EnvironmentName <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  vars <- Gen.seq (Range.linear 0 10) genVariable
  return $ Environment name vars

-- Most of the time there will be 1-5 environments, occasionally no
-- environments (to reduce discards on tests that require an environment)
genEnvironments :: Gen (HashMap EnvironmentId Environment)
genEnvironments =
  Gen.frequency
    [ (1, Gen.constant Map.empty),
      ( 5,
        fmap Map.fromList $ Gen.list (Range.linear 1 5) $ do
          eid <- EnvironmentId <$> genUUID
          env <- genEnvironment
          return (eid, env)
      )
    ]

genProjectFormState :: Gen (ProjectFormState a)
genProjectFormState = do
  name <- ProjectName <$> Gen.text (Range.linear 0 20) Gen.alphaNum
  return $ ProjectFormState name

genRequestDefFormState :: Gen (RequestDefFormState a)
genRequestDefFormState = do
  name <- RequestDefName <$> Gen.text (Range.linear 0 20) Gen.alphaNum
  url <- Url <$> Gen.text (Range.linear 0 30) Gen.alphaNum
  method <- genMethod
  body <- genRequestBody
  headers <- Gen.seq (Range.linear 0 5) genHeader
  return $ RequestDefFormState name url method body headers

genEnvironmentFormState :: Gen (EnvironmentFormState a)
genEnvironmentFormState = do
  name <- EnvironmentName <$> Gen.text (Range.linear 0 20) Gen.alphaNum
  vars <- Gen.seq (Range.linear 0 5) genVariable
  return $ EnvironmentFormState name vars

-- https://github.com/hedgehogqa/haskell-hedgehog/issues/215
genUTCTime :: Gen UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86401)
  let diff = secondsToDiffTime secs
  pure $ UTCTime day diff

-- Just use a fixed value for now, might return to this later
genNominalDiffTIme :: Gen NominalDiffTime
genNominalDiffTIme = Gen.constant (1 :: NominalDiffTime)

genResponse :: Gen Response
genResponse = do
  resBody <- ResponseBody <$> Gen.text (Range.linear 0 100) Gen.unicode
  code <- StatusCode <$> Gen.int (Range.constant 100 599)
  dateTime <- genUTCTime
  method <- genMethod
  url <- genUrl
  reqBody <- genRequestBody
  headers <- Gen.seq (Range.linear 0 10) genHeader
  elapsed <- genNominalDiffTIme
  return $ Response resBody code dateTime method url reqBody headers elapsed

genScreen :: ScreenTag -> HashMap ProjectId Project -> EnvironmentKey -> HashMap EnvironmentId Environment -> Maybe AnyScreen -> Responses -> Gen AnyScreen
genScreen tag projects env envs stashedScreen responses =
  let requireProject :: Gen (ProjectId, Project)
      requireProject = if Map.null projects then Gen.discard else Gen.element $ Map.toList projects
      requireRequestDef :: Project -> Gen (RequestDefId, RequestDef)
      requireRequestDef p = if Map.null (p ^. requestDefs) then Gen.discard else (Gen.element . Map.toList) (p ^. requestDefs)
      requireEnvironment :: Gen (EnvironmentId, Environment)
      requireEnvironment = if Map.null envs then Gen.discard else Gen.element $ Map.toList envs
      vars = case env of
        NoEnvironmentKey -> Seq.empty
        IdKey eid -> fromMaybe Seq.empty (view variables <$> Map.lookup eid envs)
   in case tag of
        ProjectListTag -> return $ AnyScreen sing $ ProjectListScreen (makeProjectList projects)
        ProjectDetailsTag -> do
          (pid, p) <- requireProject
          let c = ProjectContext pid
          return $ AnyScreen sing $ ProjectDetailsScreen c (makeRequestDefList c p)
        ProjectAddTag -> AnyScreen sing . ProjectAddScreen . makeProjectForm <$> genProjectFormState
        ProjectEditTag -> do
          (pid, _) <- requireProject
          AnyScreen sing . ProjectEditScreen (ProjectContext pid) . makeProjectForm <$> genProjectFormState
        RequestDefAddTag -> do
          (pid, _) <- requireProject
          AnyScreen sing . RequestDefAddScreen (ProjectContext pid) . makeRequestDefForm vars <$> genRequestDefFormState
        RequestDefEditTag -> do
          (pid, p) <- requireProject
          (rid, _) <- requireRequestDef p
          AnyScreen sing . RequestDefEditScreen (RequestDefContext pid rid) . makeRequestDefForm vars <$> genRequestDefFormState
        RequestDefDetailsTag -> do
          (pid, p) <- requireProject
          (rid, _) <- requireRequestDef p
          let choices = [RequestDetails, ResponseList, ResponseBodyDetails]
              ring = focusRing choices
              -- From the global map of responses, find the ones that are associated with the requestDef that
              -- we're constructing a details screen for and the environment that's selected
              localResponseMap = fromMaybe Map.empty (Map.lookup rid responses)
              localResponses = fromMaybe Seq.empty (Map.lookup env localResponseMap)
          -- If no responses exist then the top section must be selected; otherwise randomly select one of the three sections
          modifiedRing <- if Seq.null localResponses then Gen.constant ring else fmap (`focusSetCurrent` ring) (Gen.element choices)
          return $ AnyScreen sing $ RequestDefDetailsScreen (RequestDefContext pid rid) (makeResponseList localResponses) (AppFocusRing modifiedRing) Nothing
        EnvironmentAddTag ->
          AnyScreen sing . EnvironmentAddScreen . makeEnvironmentForm <$> genEnvironmentFormState
        EnvironmentListTag ->
          if isNothing stashedScreen then Gen.discard else return $ AnyScreen sing $ EnvironmentListScreen (makeEnvironmentList envs)
        EnvironmentEditTag -> do
          (eid, _) <- requireEnvironment
          AnyScreen sing . EnvironmentEditScreen (EnvironmentContext eid) . makeEnvironmentForm <$> genEnvironmentFormState
        _ -> undefined

genEnvContext :: HashMap EnvironmentId Environment -> Gen (Maybe EnvironmentContext)
genEnvContext envs =
  if Map.null envs
    then Gen.constant Nothing
    else Gen.frequency [(1, Gen.constant Nothing), (9, Just . EnvironmentContext <$> Gen.element (Map.keys envs))]

genScreenTag :: Gen ScreenTag
genScreenTag = Gen.element [ProjectListTag, ProjectDetailsTag, ProjectAddTag, ProjectEditTag, RequestDefAddTag, RequestDefDetailsTag, EnvironmentAddTag, EnvironmentListTag]

genResponseMap :: [RequestDefId] -> [EnvironmentKey] -> Gen Responses
genResponseMap rids ekeys =
  -- Creates the sub-Map for a single RequestDef, where each environment has between 0 and 10 responses
  let genSubMap :: Gen (HashMap EnvironmentKey (Seq Response)) -- [(EnvironmentKey, [Response])]
      genSubMap = fmap Map.fromList $ traverse (\e -> (e,) . Seq.fromList <$> Gen.list (Range.linear 0 10) genResponse) ekeys
   in fmap Map.fromList $ traverse (\r -> (r,) <$> genSubMap) rids

genAppState :: ScreenTag -> Gen AnyAppState
genAppState tag = do
  projects <- genProjects
  envs <- genEnvironments
  let rds = fmap (view requestDefs) (Map.elems projects)
      rids = mconcat $ Map.keys <$> rds
      envKeys = (IdKey <$> (Map.keys envs)) ++ [NoEnvironmentKey]
  responses <- genResponseMap rids envKeys
  -- currently selected environment
  envContext <- genEnvContext envs
  -- let env = envContext >>= \(EnvironmentContext eid) -> Map.lookup eid envs
  let env = case envContext of
        Just (EnvironmentContext eid) -> IdKey eid
        Nothing -> NoEnvironmentKey
  -- Generate a stashed screen that can be anything. Technically this might generate some
  -- combinations of main screen + stashed screen that won't come up when actually using
  -- the app, but they shouldn't break anything either, so we'll leave them in the tests
  stashedScreen <-
    Gen.frequency
      [ (1, Gen.constant Nothing),
        (3, genScreenTag >>= \stashedTag -> Just <$> genScreen stashedTag projects env envs Nothing responses)
      ]
  AnyScreen stag scr <- genScreen tag projects env envs stashedScreen responses
  return $ AnyAppState
    stag
    AppState
      { appStateScreen = scr,
        _appStateProjects = projects,
        _appStateEnvironments = envs,
        _appStateEnvironmentContext = envContext,
        _appStateModal = Nothing,
        _appStateMessages = Seq.empty,
        _appStateResponses = responses,
        _appStateHelpPanelVisible = HelpPanelVisible False,
        _appStateActiveRequests = Map.empty,
        _appStateStashedScreen = stashedScreen,
        _appStateCurrentTime = Nothing
      }

genKeyWithMods :: Gen (Key, [Modifier])
genKeyWithMods = do
  key <- KChar <$> Gen.alpha
  mods <- Gen.frequency [(1, Gen.constant []), (1, Gen.element [[MCtrl], [MMeta]])]
  return (key, mods)
