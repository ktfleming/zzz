import Specs.NavigationSpec (navTestTree)
import Test.Tasty
import Specs.ParsingSpec (parsingSpec)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  parsingTree <- testSpec "Parsing" parsingSpec
  defaultMain $ testGroup "All tests" [parsingTree, navTestTree]
