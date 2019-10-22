module TestUtils where

import Hedgehog (PropertyT, TestLimit, property, withTests)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

-- Shortcut to make tests less verbose
prop :: String -> PropertyT IO () -> TestTree
prop desc = testProperty desc . property

propN :: TestLimit -> String -> PropertyT IO () -> TestTree
propN limit desc = testProperty desc . withTests limit . property
