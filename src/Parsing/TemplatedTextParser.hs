{-# LANGUAGE OverloadedStrings #-}

module Parsing.TemplatedTextParser
  ( TemplatedTextPart (..),
    TemplatedText (..),
    parseTemplatedText,
  )
where

import Data.Functor (($>))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( (<|>),
    Parsec,
    anySingle,
    between,
    eof,
    lookAhead,
    many,
    some,
    someTill,
    try,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    string,
  )

data TemplatedTextPart
  = TemplateVariable T.Text -- {{variable}}
  | TextPart T.Text -- everything else
  deriving (Show, Eq)

type TemplatedTextParser = Parsec Void T.Text

newtype TemplatedText = TemplatedText [TemplatedTextPart] deriving (Show, Eq)

parseTemplatedText :: TemplatedTextParser TemplatedText
parseTemplatedText = TemplatedText <$> many (try parseTemplateVariable <|> try parseTextPart) <* eof

parseTemplateVariable :: TemplatedTextParser TemplatedTextPart
parseTemplateVariable =
  TemplateVariable . T.pack
    <$> between
      (string "{{")
      (string "}}")
      (some (alphaNumChar <|> char '_'))

parseTextPart :: TemplatedTextParser TemplatedTextPart
parseTextPart = TextPart . T.pack <$> someTill anySingle (lookAhead (string "{{" $> ()) <|> eof)
