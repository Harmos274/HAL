module Main where

-- System
import Control.Exception (handle)
import System.Environment (getArgs)
--

import Exception (exceptionHandler)
import Arguments (parseArgs, SourceCode (..), IsInterractive (..))
import Lexer (lexer)
import Parser (parse)
import Repl (repl)
import Evaluator (evaluateDefines, evaluate)

main :: IO ()
main = handle exceptionHandler $ getArgs >>= (halgo . parseArgs)

halgo :: SourceCode -> IO ()
halgo SOS                                    = print SOS
halgo (SourceCode (IsInterractive True ) fc) = mapM (fmap (parse . lexer) . readFile) fc >>= repl . evaluateDefines . concat
halgo (SourceCode (IsInterractive False) fc) = mapM (fmap (parse . lexer) . readFile) fc >>= mapM_ print . evaluate . concat
