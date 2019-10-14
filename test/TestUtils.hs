module TestUtils where

import Hedgehog (PropertyT, property)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

-- Shortcut to make tests less verbose
prop :: String -> PropertyT IO () -> TestTree
prop desc = testProperty desc . property
