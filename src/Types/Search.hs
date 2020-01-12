{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Search where

import Brick
  ( txt,
    withAttr,
  )
import Brick.Types
import Control.Lens
import Data.Coerce (coerce)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Types.Brick.Name
import Types.Models.Environment
import Types.Models.Project
import Types.Models.RequestDef
import UI.Attr (searchSectionAttr)
import UI.Editor (ZZZEditor)
import UI.List (AppList)

-- Items that can be searched for and selected
data SearchResult
  = ProjectResult ProjectContext ProjectName
  | RequestDefResult RequestDefContext ProjectName RequestDefName
  | NoEnvironmentResult
  | AnEnvironmentResult EnvironmentContext EnvironmentName
  deriving (Eq, Show)

-- Items that are translated to Widgets inside the displayed search lists
data SearchListItem = SelectableResult SearchResult | SearchSection Text | SearchBlankLine deriving (Eq, Show)

-- The fixed set of candidates that can be searched through on a screen that supports searching. Once initialized,
-- this will never change
data SearchCandidates
  = OneCandidateSet (Seq SearchResult)
  | AllCandidateSets (Seq SearchResult, Seq SearchResult, Seq SearchResult) -- (Environments, Projects, RequestDefinitions)
  deriving (Eq, Show)

-- The collection of widgets/data necessary for doing a fuzzy search
data SearchTools
  = SearchTools
      { _searchToolsEditor :: ZZZEditor,
        _searchToolsAppList :: AppList SearchListItem,
        _searchToolsCandidates :: SearchCandidates
      }
  deriving (Eq, Show)

hasHeaders :: SearchTools -> Bool
hasHeaders (SearchTools _ _ (OneCandidateSet _)) = False
hasHeaders (SearchTools _ _ (AllCandidateSets _)) = True

makeFields ''SearchTools

-- Not using the Displayable typeclass here because we need the extra Bool parameter
displaySearchListItem :: Bool -> SearchListItem -> Widget Name
displaySearchListItem fullName (SelectableResult result) = txt $ searchResultToText fullName result
displaySearchListItem _ (SearchSection t) = withAttr searchSectionAttr $ txt t
displaySearchListItem _ SearchBlankLine = txt " "

-- Check if all of the provided needles appear, in that order, in the haystack
matchAll :: [Text] -> Text -> Bool
matchAll (needle : rest) haystack = case matchOne needle haystack of
  Nothing -> False
  Just remainder -> matchAll rest remainder
matchAll [] _ = True

-- Check if needle is contained in haystack. If so, return the portion
-- of haystack after the first needle. If not, return Nothing
matchOne :: Text -> Text -> Maybe Text
matchOne needle haystack =
  let (_, remainder) = T.breakOn needle haystack
   in case remainder of
        "" -> Nothing
        _ -> Just $ T.drop (T.length needle) remainder -- the second element of `breakOn` will start with `needle`

-- Split needle on whitespace and see if all the "words" appear in haystack in that order
matchSearchText :: Text -> Text -> Bool
matchSearchText = matchAll . T.words

searchResultToText :: Bool -> SearchResult -> Text
searchResultToText _ (ProjectResult _ n) = coerce n
searchResultToText fullName (RequestDefResult _ pn rn) = if fullName then coerce pn <> " > " <> coerce rn else coerce rn
searchResultToText _ NoEnvironmentResult = "(No environment)"
searchResultToText _ (AnEnvironmentResult _ n) = coerce n

-- Filters on just one piece of the partitioned results (only environments, or only projects, etc)
filterResults' :: Bool -> Text -> Seq SearchResult -> Seq SearchResult
filterResults' fullName t = Seq.filter (matchSearchText (T.toCaseFold t) . T.toCaseFold . searchResultToText fullName)

-- Filters on all search candidates, retaining the partitioning in the case of AllCandidateSets
filterResults :: Bool -> Text -> SearchCandidates -> SearchCandidates
filterResults fullName t sc = case sc of
  OneCandidateSet cands -> OneCandidateSet $ filterResults' fullName t cands
  AllCandidateSets cands -> AllCandidateSets $ over each (filterResults' fullName t) cands
