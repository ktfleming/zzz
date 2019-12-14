{-# LANGUAGE OverloadedStrings #-}

module Specs.ParsingSpec where

import qualified Data.Text as T
import Parsing.TemplatedTextParser
import Test.Hspec
import Text.Megaparsec (parse)
import Types.Models.Environment
  ( Variable (..),
    VariableName (..),
    VariableValue (..),
  )
import Types.Search
  ( matchOne,
    matchSearchText,
  )
import Utils.Text (substitute)

parsingSpec :: Spec
parsingSpec = do
  describe "matchOne" $ it "should return the proper remainder" $ do
    matchOne "abc" "abcdefg" `shouldBe` Just "defg"
    matchOne "abc" "foo" `shouldBe` Nothing
    matchOne "abc" "aaaabcdefg" `shouldBe` Just "defg"
  describe "matchSearchText" $ it "should match correctly" $ do
    matchSearchText "one two three" "one abc two def three" `shouldBe` True
    matchSearchText "one two three" "onetwothree" `shouldBe` True
    matchSearchText "one two four" "one abc two def three" `shouldBe` False
    matchSearchText "one two three" "three two one" `shouldBe` False
  describe "substitute" $ it "should subsitute correctly" $ do
    let vars =
          [ Variable (VariableName "host") (VariableValue "http://www.example.com"),
            Variable (VariableName "port") (VariableValue "8080")
          ]
    substitute vars "{{host}}:{{port}}/test" `shouldBe` ("http://www.example.com:8080/test" :: T.Text)
    substitute vars "{{port}}{{port}}" `shouldBe` ("80808080" :: T.Text)
    substitute vars "no_vars" `shouldBe` ("no_vars" :: T.Text)
    substitute vars "{port}" `shouldBe` ("{port}" :: T.Text)
    substitute vars "{{{port}}}" `shouldBe` ("{8080}" :: T.Text)
  describe "TemplatedTextParser" $ it "should parse correctly" $ do
    let go :: T.Text -> [TemplatedTextPart] -> Expectation
        go t parts = parse parseTemplatedText "" t `shouldBe` Right (TemplatedText parts)
    go "just a string" [TextPart "just a string"]
    go "{{variable}}" [TemplateVariable "variable"]
    go "text1{{var}}text2" [TextPart "text1", TemplateVariable "var", TextPart "text2"]
    go "text1{{var}}" [TextPart "text1", TemplateVariable "var"]
    go "{{var}}text1" [TemplateVariable "var", TextPart "text1"]
    go "{text}" [TextPart "{text}"]
    go "{{text" [TextPart "{{text"]
    go "{{text}" [TextPart "{{text}"]
    go "{{ab^c}}" [TextPart "{{ab^c}}"]
    go "" []
