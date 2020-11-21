module Evaluator
    ( evaluate,
      Value (..),
    ) where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Exception (throw)

import qualified Data.Map.Strict as Map

import Parser (Expression (..))
import Exception (HExceptions (EvaluationException))

type Context = Map.Map String Value

data Function = Defined [String] Expression | Builtin ([Value] -> Value) | Spe (Context -> [Expression] -> Value)
data Value = Function Function | Number Int | String String | List [Value] | Nil

instance Show Value where
    show (Function _) = "#<procedure>"
    show (Number n)   = show n
    show (String s)   = s
    show (List   l)   = Evaluator.showList l
    show Nil          = "()"

showList :: [Value] -> String
showList []         = "()"
showList [x, Nil]   = '(' : show x ++ ")"
showList (first:xs) = '(' : show first ++ showList' xs

showList' :: [Value] -> String
showList' [v, Nil] = (' ': show v) ++ ")"
showList' [v]      = (" . " ++ show v) ++ ")"
showList' (v:xs) = (' ' : show v) ++ showList' xs
showList' []     = ")"

evaluate :: [Expression] -> [Value]
evaluate = evaluate' baseContext

evaluate' :: Context -> [Expression] -> [Value]
evaluate' _ []        = []
evaluate' c (Seq (Atom "define" : define) : xs) = evaluate'' (evaluateDefine c define) xs
evaluate' c (expr:xs)                           = evaluateExpr c expr : evaluate' c xs

evaluate'' :: (Context, String) -> [Expression] -> [Value]
evaluate'' (c, x) exprs = String x : evaluate' c exprs

evaluateDefine :: Context -> [Expression] -> (Context, String)
evaluateDefine c [Atom symbol, expr]              = (Map.insert symbol (evaluateExpr c expr) c, symbol)
evaluateDefine c [Seq (Atom symbol : args), func] = (Map.insert symbol (createFunction args func) c, symbol)

createFunction :: [Expression] -> Expression -> Value
createFunction args func = Function $ Defined (map asAtom args) func

evaluateExpr :: Context -> Expression -> Value
evaluateExpr _ (Quoted expr) = evaluateQuoted expr
evaluateExpr c (Atom atom)   = evaluateAtom c atom
evaluateExpr c (Seq exprs)   = evaluateSeq c exprs

evaluateAtom :: Context -> String -> Value
evaluateAtom c s = Map.lookup s c
                ?: ((Number <$> readMaybe s)
                ?: String s)

evaluateSeq :: Context -> [Expression] -> Value
evaluateSeq _ []        = Nil
evaluateSeq c (expr:xs) = evaluateSeq' c (evaluateExpr c expr) xs

evaluateSeq' :: Context -> Value -> [Expression] -> Value
evaluateSeq' c (Function (Spe s)) exprs = s c exprs
evaluateSeq' c v exprs                  = evaluateSeq'' c $ v:map (evaluateExpr c) exprs

evaluateSeq'' :: Context -> [Value] -> Value
evaluateSeq'' c (Function f : xs) = invokeFunction c f xs
evaluateSeq'' _ []                = Nil
evaluateSeq'' _ _                 = throw $ EvaluationException "Sequence is not a procedure"

evaluateQuoted :: Expression -> Value
evaluateQuoted (Atom a)   = evaluateQuotedAtom a
evaluateQuoted (Seq  [])  = Nil
evaluateQuoted (Seq  q)   = List $ evaluateQuotedSeq q
evaluateQuoted (Quoted q) = evaluateQuoted q

evaluateQuotedAtom :: String -> Value
evaluateQuotedAtom s = (Number <$> readMaybe s) ?: String s

evaluateQuotedSeq :: [Expression] -> [Value]
evaluateQuotedSeq = foldr ((:) . evaluateQuoted) [Nil]

invokeFunction :: Context -> Function -> [Value] -> Value
invokeFunction _ (Builtin b) args            = b args
invokeFunction c (Defined symbols func) args = evaluateExpr (functionContext c symbols args) func

functionContext :: Context -> [String] -> [Value] -> Context
functionContext c (symbol:sxs) (value:vxs) = functionContext (Map.insert symbol value c) sxs vxs
functionContext c []           []          = c
functionContext _ _            _           = throw $ EvaluationException "Invalid number of arguments"

baseContext :: Context
baseContext = Map.fromList builtins

builtins :: [(String, Value)]
builtins = [("+",      Function $ Builtin add),
            ("-",      Function $ Builtin sub),
            ("*",      Function $ Builtin mult),
            ("div",    Function $ Builtin division),
            ("mod",    Function $ Builtin modulo),
            ("<",      Function $ Builtin inferior),
            ("eq?",    Function $ Builtin eq),
            ("atom?",  Function $ Builtin atom),
            ("cons",   Function $ Builtin cons),
            ("car",    Function $ Builtin car),
            ("cdr",    Function $ Builtin cdr),
            ("cond",   Function $ Spe cond),
            ("lambda", Function $ Spe lambda),
            ("let"   , Function $ Spe slet),
            ("quote" , Function $ Spe quote)
           ]

add :: [Value] -> Value
add = Number . sum . map asNumber

sub :: [Value] -> Value
sub [Number n]       = Number $ -n
sub (Number init:xs) = Number $ foldl (-) init $ map asNumber xs

mult :: [Value] -> Value
mult = Number . product . map asNumber

division :: [Value] -> Value
division [Number lhs, Number rhs] = Number $ quot lhs rhs

modulo :: [Value] -> Value
modulo [lhs, rhs] = Number $ mod (asNumber lhs) (asNumber rhs)

inferior :: [Value] -> Value
inferior [lhs, rhs] = fromBool $ (<) (asNumber lhs) (asNumber rhs)

cons :: [Value] -> Value
cons [List l, Nil] = List l
cons [lhs, List l] = List $ lhs:l
cons [lhs, rhs] = List [lhs, rhs]

car :: [Value] -> Value
car [List (f : _)] = f
car _              = throw $ EvaluationException "car : Invalid arguments"

cdr :: [Value] -> Value
cdr [List [_, v]]   = v
cdr [List (_ : l)]  = List l
cdr _               = throw $ EvaluationException "cdr : Invalid arguments"

cond :: Context -> [Expression] -> Value
cond c (Seq (expr : ret : _) : xs) = cond' c (evaluateExpr c expr) ret xs

cond' :: Context -> Value -> Expression -> [Expression] -> Value
cond' c (String "#f") _   xs = cond c xs
cond' c _             ret _  = evaluateExpr c ret

eq :: [Value] -> Value
eq (Number lhs : Number rhs : _) | lhs == rhs = fromBool True
eq (String lhs : String rhs : _) | lhs == rhs = fromBool True
eq (Nil        : Nil        : _)              = fromBool True
eq _                                          = fromBool False

atom :: [Value] -> Value
atom []       = throw $ EvaluationException "atom? : no argument"
atom [List _] = fromBool False
atom _        = fromBool True

lambda :: Context -> [Expression] -> Value
lambda _ (args : func : _) = lambda' args func
lambda _ _                 = throw $ EvaluationException "lambda : Invalid arguments"

lambda' :: Expression -> Expression -> Value
lambda' (Seq args) func = Function $ Defined (map asAtom args) func
lambda' _ _             = throw $ EvaluationException "lambda : Invalid arguments"

slet :: Context -> [Expression] -> Value
slet c (Seq defs : expr : _) = evaluateExpr (letContext c defs) expr
slet _ _                     = throw $ EvaluationException "let : Invalid arguments"

letContext :: Context -> [Expression] -> Context
letContext c (Seq (key : value : _) : xs) = letContext (Map.insert (asAtom key) (evaluateExpr c value) c) xs
letContext c []                           = c

quote :: Context -> [Expression] -> Value
quote _ [expr] = evaluateQuoted expr
quote _ _      = throw $ EvaluationException "quote : Invalid arguments"

fromBool :: Bool -> Value
fromBool True  = String "#t"
fromBool False = String "#f"

asAtom :: Expression -> String
asAtom (Atom a) = a
asAtom _        = throw $ EvaluationException "Invalid atom"

asNumber :: Value -> Int
asNumber (Number n) = n
asNumber v          = throw $ EvaluationException $ show v ++ " is not a number"

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe
