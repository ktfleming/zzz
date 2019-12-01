import Specs.Navigation.Environments (environmentNavTree)
import Specs.Navigation.Projects (projectNavTestTree)
import Specs.Navigation.RequestDefs (requestDefNavTree)
import Specs.ParsingSpec (parsingSpec)
import Test.Tasty
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  parsingTree <- testSpec "Parsing" parsingSpec
  defaultMain $ testGroup "All tests" [parsingTree, projectNavTestTree, requestDefNavTree, environmentNavTree]
