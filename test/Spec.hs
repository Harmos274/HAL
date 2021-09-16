import Test.Tasty (defaultMain, TestTree, testGroup)

import LexerTests     (lexerTests)
import ParserTests    (parserTests)
import EvaluatorTests (evaluatorTests)

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Hal Tests" [lexerTests, parserTests, evaluatorTests]