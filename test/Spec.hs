{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Types.Search (matchOne, matchSearchText)

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