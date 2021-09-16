module EvaluatorTests
    ( evaluatorTests
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

import Prelude hiding (div, mod)

import Lexer (lexer)
import Parser (parse)
import Evaluator (evaluate)

deusExMachina :: String -> String
deusExMachina = show . evaluate . parse . lexer

evaluatorTests :: TestTree
evaluatorTests = testGroup "Evaluator" [emptyExpression, true, false, arithmeticBuiltins, baseBuiltins, specialFormBuiltins]

emptyExpression :: TestTree
emptyExpression = testCase "Empty Expression" $
                    assertEqual "Empty Epression should ouput empty Value"
                        (deusExMachina "")
                        "[]"

true :: TestTree
true = testCase "#t" $
        assertEqual "#t shoudl exist as True boolean"
            (deusExMachina "#t")
            "[#t]"
false :: TestTree
false = testCase "#f" $
        assertEqual "#f shoudl exist as True boolean"
            (deusExMachina "#f")
            "[#f]"

arithmeticBuiltins :: TestTree
arithmeticBuiltins = testGroup "Arithmetic Builtins" [add, sub, mul, div, mod]

add :: TestTree
add = testCase "+" $
        assertEqual "Add should be an addition"
            (deusExMachina "(+ 1 1)")
            "[2]"
sub :: TestTree
sub = testCase "-" $
        assertEqual "Sub should be a substraction"
            (deusExMachina "(- 1 1)")
            "[0]"

mul :: TestTree
mul = testCase "*" $
        assertEqual "Mul should be a multiplication"
            (deusExMachina "(* 1 2)")
            "[2]"

div :: TestTree
div = testCase "div" $
        assertEqual "Div should be a division"
            (deusExMachina "(div 4 2)")
            "[2]"

mod :: TestTree
mod =  testCase "mod" $
           assertEqual "Mod should be a division"
               (deusExMachina "(mod 4 2)")
               "[0]"

baseBuiltins :: TestTree
baseBuiltins = testGroup "Basic Builtins" [cons, car, cdr, eq, atom]

cons :: TestTree
cons = testCase "cons" $
           assertEqual "Cons should build a list"
               (deusExMachina "(cons 4 2)")
               "[(4 . 2)]"

car :: TestTree
car = testCase "car" $
           assertEqual "Car should return the head of the list"
               (deusExMachina "(car (cons 4 2))")
               "[4]"

cdr :: TestTree
cdr = testCase "cdr" $
        assertEqual "Cdr should return the second element of a list"
            (deusExMachina "(cdr (cons 1 2))")
            "[2]"

eq :: TestTree
eq = testCase "eq?" $
        assertEqual "Eq? should return true on equivalence"
            (deusExMachina "(eq? 2 2)")
            "[#t]"

atom :: TestTree
atom = testCase "atom?" $
            assertEqual "Atom? should return true atomic element"
                (deusExMachina "(atom? 'foo)")
                "[#t]"

specialFormBuiltins :: TestTree
specialFormBuiltins = testGroup "Special Form Builtins" [quote, lambda, define, lispLet, cond]

quote :: TestTree
quote = testCase "quote" $
            assertEqual "Quote should return a non evaluated argument"
                (deusExMachina "(quote `+)")
                "[+]"

lambda :: TestTree
lambda = testCase "lambda" $
            assertEqual "Lambda should create a procédure"
                (deusExMachina "((lambda (a b) (+ a b)) 1 2)")
                "[3]"

define :: TestTree
define = testCase "define" $
            assertEqual "Define should be quiet and not printable"
                (deusExMachina "(define foo 42) foo")
                "[42]"

lispLet :: TestTree
lispLet = testCase "let" $
            assertEqual "Let should evaluate a procedure with scopped argument"
                (deusExMachina "(let ((a 2) (b (+ 1 2))) (+ a b))")
                "[5]"

cond :: TestTree
cond = testCase "cond" $
            assertEqual "Cond should evaluate a list of tuple and return the second element of the first true"
                (deusExMachina "(cond (#f 1) (#t (+ 1 1)))")
                "[2]"