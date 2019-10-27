{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Gens where

import Brick.Focus (focusRing, focusSetCurrent)
import Control.Lens
import Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
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
    HelpPanelVisible (..),
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
import UI.FocusRing (AppFocusRing (..))
import UI.Projects.Add (makeProjectAddForm)
import UI.Projects.Details (makeRequestDefList)
import UI.Projects.Edit (makeProjectEditForm)
import UI.Projects.List (makeProjectList)
import UI.RequestDefs.Add (makeRequestDefAddForm)
import UI.RequestDefs.Details (makeResponseList)
import UI.RequestDefs.Edit (makeRequestDefEditForm)

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

genEnvironments :: Gen (HashMap EnvironmentId Environment)
genEnvironments = fmap Map.fromList $ Gen.list (Range.linear 0 10) $ do
  eid <- EnvironmentId <$> genUUID
  env <- genEnvironment
  return (eid, env)

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

genScreen :: ScreenTag -> HashMap ProjectId Project -> Maybe Environment -> Gen AnyScreen
genScreen tag projects env =
  let requireProject :: Gen (ProjectId, Project)
      requireProject = if Map.null projects then Gen.discard else Gen.element $ Map.toList projects
      requireRequestDef :: Project -> Gen (RequestDefId, RequestDef)
      requireRequestDef p = if Map.null (p ^. requestDefs) then Gen.discard else (Gen.element . Map.toList) (p ^. requestDefs)
   in case tag of
        ProjectListTag -> return $ AnyScreen sing $ ProjectListScreen (makeProjectList projects)
        ProjectDetailsTag -> do
          (pid, p) <- requireProject
          let c = ProjectContext pid
          return $ AnyScreen sing $ ProjectDetailsScreen c (makeRequestDefList c p)
        ProjectAddTag -> AnyScreen sing . ProjectAddScreen . makeProjectAddForm <$> genProjectFormState
        ProjectEditTag -> do
          (pid, _) <- requireProject
          AnyScreen sing . ProjectEditScreen (ProjectContext pid) . makeProjectEditForm <$> genProjectFormState
        RequestDefAddTag -> do
          (pid, _) <- requireProject
          AnyScreen sing . RequestDefAddScreen (ProjectContext pid) . makeRequestDefAddForm <$> genRequestDefFormState
        RequestDefEditTag -> do
          (pid, p) <- requireProject
          (rid, _) <- requireRequestDef p
          let vars = maybe Seq.empty (view variables) env
          AnyScreen sing . RequestDefEditScreen (RequestDefContext pid rid) . makeRequestDefEditForm vars <$> genRequestDefFormState
        RequestDefDetailsTag -> do
          (pid, p) <- requireProject
          (rid, _) <- requireRequestDef p
          responses <- Gen.seq (Range.linear 0 10) genResponse
          let choices = [RequestDetails, ResponseList, ResponseBodyDetails]
              ring = focusRing choices
          -- If no responses exist then the top section must be selected; otherwise randomly select one of the three sections
          modifiedRing <- if Seq.null responses then Gen.constant ring else fmap (`focusSetCurrent` ring) (Gen.element choices)
          return $ AnyScreen sing $ RequestDefDetailsScreen (RequestDefContext pid rid) (makeResponseList responses) (AppFocusRing modifiedRing) Nothing
        _ -> undefined

genEnvContext :: HashMap EnvironmentId Environment -> Gen (Maybe EnvironmentContext)
genEnvContext envs =
  if Map.null envs
    then Gen.constant Nothing
    else Gen.frequency [(1, Gen.constant Nothing), (9, Just . EnvironmentContext <$> Gen.element (Map.keys envs))]

genAppState :: ScreenTag -> Gen AnyAppState
genAppState tag = do
  projects <- genProjects
  envs <- genEnvironments
  envContext <- genEnvContext envs
  let env = envContext >>= \(EnvironmentContext eid) -> Map.lookup eid envs
  AnyScreen stag scr <- genScreen tag projects env
  return $ AnyAppState
    stag
    AppState
      { appStateScreen = scr,
        _appStateProjects = projects,
        _appStateEnvironments = envs,
        _appStateEnvironmentContext = envContext,
        _appStateModal = Nothing,
        _appStateMessages = Seq.empty,
        _appStateResponses = Map.empty,
        _appStateHelpPanelVisible = HelpPanelVisible False,
        _appStateActiveRequests = Map.empty,
        _appStateStashedScreen = Nothing,
        _appStateCurrentTime = Nothing
      }

genKeyWithMods :: Gen (Key, [Modifier])
genKeyWithMods = do
  key <- KChar <$> Gen.alpha
  mods <- Gen.frequency [(1, Gen.constant []), (1, Gen.element [[MCtrl], [MMeta]])]
  return (key, mods)
