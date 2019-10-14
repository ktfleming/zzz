{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Gens where

import Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import Data.Singletons (sing)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.IO (unsafePerformIO)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Types.AppState
  ( AnyAppState (..),
    AppState (..),
    HelpPanelVisible (..),
  )
import Types.Methods (Method (..))
import Types.Models.Environment
  ( Environment (..),
    EnvironmentContext (..),
    EnvironmentName (..),
    Variable (..),
    VariableName (..),
    VariableValue (..),
  )
import Types.Models.Header
import Types.Models.Id
import Types.Models.Project
import Types.Models.RequestDef
import Types.Models.Screen
import Types.Models.Url (Url (..))
import UI.Projects.Details (makeRequestDefList)
import UI.Projects.List (makeProjectList)

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

genScreen :: ScreenTag -> HashMap ProjectId Project -> Gen AnyScreen
genScreen tag projects = case tag of
  ProjectListTag -> return $ AnyScreen sing $ ProjectListScreen (makeProjectList projects)
  ProjectDetailsTag ->
    if Map.null projects
      then Gen.discard -- need at least one project
      else do
        (pid, p) <- Gen.element $ Map.toList projects
        let c = ProjectContext pid
        return $ AnyScreen sing $ ProjectDetailsScreen c (makeRequestDefList c p)
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
  AnyScreen stag scr <- genScreen tag projects
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
