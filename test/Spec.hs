{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Types.Search (matchOne, matchSearchText)
import Utils.Text (substitute)
import Types.Models.Environment (Variable(..), VariableName(..), VariableValue(..))
import Parsing.TemplatedUrlParser
import Text.Megaparsec (parse)
import qualified Data.Text as T

main :: IO ()
main =
  hspec $ do
    describe "matchOne" $
      it "should return the proper remainder" $ do
        matchOne "abc" "abcdefg" `shouldBe` Just "defg"
        matchOne "abc" "foo" `shouldBe` Nothing
        matchOne "abc" "aaaabcdefg" `shouldBe` Just "defg"
    describe "matchSearchText" $
      it "should match correctly" $ do
        matchSearchText "one two three" "one abc two def three" `shouldBe` True
        matchSearchText "one two three" "onetwothree" `shouldBe` True
        matchSearchText "one two four" "one abc two def three" `shouldBe` False
        matchSearchText "one two three" "three two one" `shouldBe` False
    describe "substitute" $
      it "should subsitute correctly" $ do
        let vars =
              [ Variable {variableName = VariableName "host", variableValue = VariableValue "http://www.example.com"}
              , Variable {variableName = VariableName "port", variableValue = VariableValue "8080"}
              ]
        substitute "{{host}}:{{port}}/test" vars `shouldBe` "http://www.example.com:8080/test"
        substitute "{{port}}{{port}}" vars `shouldBe` "80808080"
        substitute "no_vars" vars `shouldBe` "no_vars"
        substitute "{port}" vars `shouldBe` "{port}"
        substitute "{{{port}}}" vars `shouldBe` "{8080}"
    describe "TemplatedUrlParser" $
      it "should parse correctly" $ do
        let go :: T.Text -> [TemplatedUrlPart] -> Expectation
            go t parts = parse parseTemplatedUrl "" t `shouldBe` Right (TemplatedUrl parts)

        go "just a string"     [TextPart "just a string"]
        go "{{variable}}"      [TemplateVariable "variable"]
        go "text1{{var}}text2" [TextPart "text1", TemplateVariable "var", TextPart "text2"]
        go "text1{{var}}"      [TextPart "text1", TemplateVariable "var"]
        go "{{var}}text1"      [TemplateVariable "var", TextPart "text1"]
        go "{text}"            [TextPart "{text}"]
        go "{{text"            [TextPart "{{text"]
        go "{{text}"           [TextPart "{{text}"]
        go "{{ab^c}}"          [TextPart "{{ab^c}}"]
        go ""                  []
