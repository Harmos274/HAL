module ParserTests
    ( parserTests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Control.Exception.Base (evaluate)

import AssertExeption (assertHException)
import Exception (HExceptions (ParsingException))
import Parser (parse, Expression (..))
import Lexer (Token (..))

newtype TestExpr = TestExpr Expression deriving (Show)

instance Eq TestExpr where
    TestExpr (Atom v)   == TestExpr (Atom v2)   = v == v2
    TestExpr (Quoted e) == TestExpr (Quoted e2) = TestExpr e == TestExpr e2
    TestExpr (Seq e)    == TestExpr (Seq e2)    = map TestExpr e == map TestExpr e2
    _                   == _                    = False

ttParse :: [Token] -> [TestExpr]
ttParse = map TestExpr . parse

parserTests :: TestTree
parserTests = testGroup "Parser" [emptyList, singleValue, singleSequence, singleQuoted, simpleDefine, complexeDefine,
                                  unmatchedParenthesis, unmatchedClosingParenthesis, emptyExpression]

emptyList :: TestTree
emptyList = testCase "Empty Token List" $
                assertEqual "Empty list should output empty array"
                    (ttParse [])
                    []

singleValue :: TestTree
singleValue = testCase "Single Word"  $
                assertEqual "A single word should be translated as an Atom"
                    (ttParse [Word "foo"])
                    [TestExpr $ Atom "foo"]

singleSequence :: TestTree
singleSequence = testCase "Single Bracket Surrounded" $
                    assertEqual "Parenthesis surrounded tokens should be translated as Sequences"
                        (ttParse [OpenParen, Word "foo", Word "bar", CloseParen])
                        (map TestExpr [Seq [Atom "foo", Atom "bar"]])

singleQuoted :: TestTree
singleQuoted = testCase "Single Quoted Word" $
                    assertEqual "Quoted Word should be translated as a Quoted"
                        (ttParse [Quote, Word "foo"])
                        (map TestExpr [Quoted $ Atom "foo"])

simpleDefine :: TestTree
simpleDefine = testCase "Simple Define" $
                 assertEqual "A define should be a Seq"
                     (ttParse [OpenParen, Word "define", Word "foo", Word "42", CloseParen])
                     (map TestExpr [Seq [Atom "define", Atom "foo", Atom "42"]])

complexeDefine :: TestTree
complexeDefine = testCase "Complexe Define" $
                 assertEqual "A define should be a Seq"
                     (ttParse [OpenParen, Word "define", OpenParen, Word "add", Word "a", Word "b", CloseParen, OpenParen, Word "+", Word "a", Word "b", CloseParen, CloseParen])
                     (map TestExpr [Seq [Atom "define", Seq [Atom "add", Atom "a", Atom "b"], Seq[Atom "+", Atom "a", Atom "b"]]])

unmatchedParenthesis :: TestTree
unmatchedParenthesis = testCase "Unmatched Open Parenthesis" $
                         assertHException
                            (ParsingException "Unmatched opening parenthesis")
                            (evaluate $ ttParse [OpenParen])

unmatchedClosingParenthesis :: TestTree
unmatchedClosingParenthesis = testCase "Unmatched Close Parenthesis" $
                                 assertHException
                                    (ParsingException "Unmatched closing parenthesis")
                                    (evaluate $ ttParse [CloseParen])

emptyExpression :: TestTree
emptyExpression = testCase "Empty Quote Expression" $
                     assertHException
                        (ParsingException "Empty expression")
                        (evaluate $ ttParse [Quote])