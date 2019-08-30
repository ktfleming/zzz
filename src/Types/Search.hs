{-# LANGUAGE OverloadedStrings #-}

module Types.Search where

import           Data.Coerce                    ( coerce )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import           Types.Classes.Displayable
import           Types.Models.Project           ( ProjectContext
                                                , ProjectName(..)
                                                )
import           Types.Models.RequestDef        ( RequestDefContext
                                                , RequestDefName(..)
                                                )

data SearchResult =
    ProjectResult ProjectName ProjectContext
  | RequestDefResult ProjectName RequestDefName RequestDefContext

instance Displayable SearchResult where
  display (ProjectResult projectName _          ) = coerce projectName
  display (RequestDefResult projectName rdName _) = coerce projectName <> " > " <> coerce rdName

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

filterResults :: T.Text -> Seq SearchResult -> Seq SearchResult
filterResults t = S.filter (matchSearchText (T.toCaseFold t) . T.toCaseFold . display)
