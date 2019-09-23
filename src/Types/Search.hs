{-# LANGUAGE OverloadedStrings #-}

module Types.Search where

import           Brick                          (

                                                 txt
                                                , withAttr
                                                )
import           Control.Lens
import           Data.Coerce                    ( coerce )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           Types.Classes.Displayable
import           Types.Models.Environment       ( EnvironmentContext
                                                , EnvironmentName(..)
                                                )
import           Types.Models.Project           ( ProjectContext
                                                , ProjectName(..)
                                                )
import           Types.Models.RequestDef        ( RequestDefContext
                                                , RequestDefName(..)
                                                )
import           UI.Attr                        ( searchSectionAttr )

data SearchResult =
    ProjectResult ProjectName ProjectContext
  | RequestDefResult ProjectName RequestDefName RequestDefContext
  | EnvironmentResult EnvironmentName EnvironmentContext

data SearchListItem = SelectableResult SearchResult | SearchSection T.Text | SearchBlankLine

instance Displayable SearchListItem where
  display (SelectableResult result) = txt $ searchResultToText result
  display (SearchSection    t     ) = withAttr searchSectionAttr $ txt t
  display SearchBlankLine           = txt " "

-- Check if all of the provided needles appear, in that order, in the haystack
matchAll :: [T.Text] -> T.Text -> Bool
matchAll (needle : rest) haystack = case matchOne needle haystack of
  Nothing        -> False
  Just remainder -> matchAll rest remainder
matchAll [] _ = True

-- Check if needle is contained in haystack. If so, return the portion
-- of haystack after the first needle. If not, return Nothing
matchOne :: T.Text -> T.Text -> Maybe T.Text
matchOne needle haystack =
  let (_, remainder) = T.breakOn needle haystack
  in  case remainder of
        "" -> Nothing
        _  -> Just $ T.drop (T.length needle) remainder -- the second element of `breakOn` will start with `needle`

-- Split needle on whitespace and see if all the "words" appear in haystack in that order
matchSearchText :: T.Text -> T.Text -> Bool
matchSearchText = matchAll . T.words

searchResultToText :: SearchResult -> T.Text
searchResultToText (ProjectResult n _       ) = coerce n
searchResultToText (RequestDefResult pn rn _) = coerce pn <> " > " <> coerce rn
searchResultToText (EnvironmentResult en _  ) = coerce en

type PartitionedResults = (Seq SearchResult, Seq SearchResult, Seq SearchResult) -- (Environments, Projects, RequestDefinitions)

-- Filters on just one piece of the partitioned results (only environments, or only projects, etc)
filterResults' :: T.Text -> Seq SearchResult -> Seq SearchResult
filterResults' t = S.filter (matchSearchText (T.toCaseFold t) . T.toCaseFold . searchResultToText)

-- Filters on the entire tuple of of partitioned results, retaining the partitioning
filterResults :: T.Text -> PartitionedResults -> PartitionedResults
filterResults t = over each (filterResults' t)
