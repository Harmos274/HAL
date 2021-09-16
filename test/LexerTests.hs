module LexerTests
    ( lexerTests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Control.Exception.Base (evaluate)

import Lexer (lexer, Token (..))
import AssertExeption (assertHException)
import Exception (HExceptions (LexingException))

newtype TestToken = TestToken Token deriving (Show)

instance Eq TestToken where
    TestToken OpenParen   == TestToken OpenParen    = True
    TestToken CloseParen  == TestToken CloseParen   = True
    TestToken Quote       == TestToken Quote        = True
    TestToken (Word s)    == TestToken (Word s2)    = s == s2
    TestToken (Comment s) == TestToken (Comment s2) = s == s2
    _                     == _                      = False

ttLexer :: String -> [TestToken]
ttLexer = map TestToken . lexer

lexerTests :: TestTree
lexerTests = testGroup "Lexer" [emptyString, simpleWord, stringLiteral, allToken, withWhitespaces, unmatchedDoubleQuotes]

emptyString :: TestTree
emptyString = testCase "Empty String" $
                    assertEqual "Empty string should output empty array"
                        (ttLexer "") []

simpleWord :: TestTree
simpleWord = testCase "Simple word" $
                    assertEqual "A single string without () should output a word"
                        (ttLexer "foo") [TestToken $ Word "foo"]

stringLiteral :: TestTree
stringLiteral = testCase "String litteral" $
                    assertEqual "Should output a quoted word with the content of the string"
                        (ttLexer "\"string literal\"")
                        (map TestToken [Quote, Word "string literal"])

allToken :: TestTree
allToken = testCase "All Tokens" $
                    assertEqual "Should output a list with all the tokens of Token datatype"
                        (ttLexer "(define foo) '`foo \"string literal\" ;; comment")
                        (map TestToken [OpenParen, Word "define", Word "foo", CloseParen, Quote, Quote, Word "foo", Quote, Word "string literal", Comment "; comment"])

withWhitespaces :: TestTree
withWhitespaces = testCase "Separation with whitespaces" $
                    assertEqual "Whitespace should be ignored at parsing"
                        (ttLexer "\t      foo    (2   )\r a\n")
                        (map TestToken  [Word "foo", OpenParen , Word "2", CloseParen, Word "a"])

unmatchedDoubleQuotes :: TestTree
unmatchedDoubleQuotes = testCase "Unmatched double quotes" $
                          assertHException
                            (LexingException "Unmatched quote")
                            (evaluate $ lexer "\"invalid string")