{-# LANGUAGE OverloadedStrings #-}

module Parsing.TemplatedUrlParser
  ( TemplatedUrlPart(..)
  , TemplatedUrl(..)
  , parseTemplatedUrl
  )
where

import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , anySingle
                                                , between
                                                , eof
                                                , lookAhead
                                                , many
                                                , some
                                                , someTill
                                                , try
                                                , (<|>)
                                                )
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , string
                                                )

data TemplatedUrlPart =
    TemplateVariable T.Text -- {{variable}}
  | TextPart T.Text         -- everything else
  deriving (Show, Eq)

type TemplatedUrlParser = Parsec Void T.Text

newtype TemplatedUrl = TemplatedUrl [TemplatedUrlPart] deriving (Show, Eq)

parseTemplatedUrl :: TemplatedUrlParser TemplatedUrl
parseTemplatedUrl = TemplatedUrl <$> many (try parseTemplateVariable <|> try parseTextPart) <* eof

parseTemplateVariable :: TemplatedUrlParser TemplatedUrlPart
parseTemplateVariable = TemplateVariable . T.pack <$> between (string "{{")
                                                              (string "}}")
                                                              (some (alphaNumChar <|> char '_'))

parseTextPart :: TemplatedUrlParser TemplatedUrlPart
parseTextPart = TextPart . T.pack <$> someTill anySingle (lookAhead (string "{{" $> ()) <|> eof)
